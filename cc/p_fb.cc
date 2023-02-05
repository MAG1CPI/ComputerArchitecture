#include "cache.h"
#include "ooo_cpu.h"

/******************GHB*******************/
#define IT_SIZE 256
#define GHB_SIZE 256
#define INVALID_INDEX -1

struct GHBEntry {
    uint64_t cl_addr;
    int16_t prev;
};

int16_t IT[IT_SIZE];     // Index Table
GHBEntry GHB[GHB_SIZE];  // GHB

int16_t cur_index = 0;

int16_t GHB_lookahead = 1;
int16_t GHB_degree = 10;
/****************Feedback****************/
#define SAMPLE_INTERVAL 1000
#define ACCURACY_THRESHOLD_H 0.75
#define ACCURACY_THRESHOLD_L 0.4
#define LATENESS_THRESHOLD 0.01
#define COUNTER_MIN_VAL 1
#define COUNTER_MAX_VAL 5

enum accuracy_state_t {
    ACCURACY_LOW,
    ACCURACY_MEDIUM,
    ACCURACY_HIGH,
    NUM_ACCURACY_STATE
};

enum lateness_state_t {
    LATE,
    NOT_LATE,
    NUM_LATENESS_STATE
};

enum aggressivness_t {
    VERY_CONSERVATIVE,
    CONSERVATIVE,
    MIDDLE,
    AGGRESSIVE,
    VERY_AGGRESSIVE,
    NUM_AGGRESSIVNESS
};

// init prefetcher configuration
int32_t GHB_lookahead_table[NUM_AGGRESSIVNESS];
int32_t GHB_degree_table[NUM_AGGRESSIVNESS];
// global counter to determine prefetcher aggressiveness
int32_t counter = 3;
// table to encode counter updates
int32_t counter_update[NUM_ACCURACY_STATE][NUM_LATENESS_STATE];

void CACHE::l2c_prefetcher_initialize() {
    std::cout << "CPU " << cpu << " L2C GHB prefetcher" << std::endl;
    // init IT & GHB
    for (uint32_t i = 0; i < IT_SIZE; i++) {
        IT[i] = INVALID_INDEX;
    }
    for (uint32_t i = 0; i < GHB_SIZE; i++) {
        GHB[i].cl_addr = 0;
        GHB[i].prev = INVALID_INDEX;
    }
    // init GHB_lookahead_table & GHB_degree_table
    GHB_lookahead_table[VERY_CONSERVATIVE] = 1;
    GHB_degree_table[VERY_CONSERVATIVE] = 4;

    GHB_lookahead_table[CONSERVATIVE] = 1;
    GHB_degree_table[CONSERVATIVE] = 6;

    GHB_lookahead_table[MIDDLE] = 1;
    GHB_degree_table[MIDDLE] = 10;

    GHB_lookahead_table[AGGRESSIVE] = 2;
    GHB_degree_table[AGGRESSIVE] = 16;

    GHB_lookahead_table[VERY_AGGRESSIVE] = 2;
    GHB_degree_table[VERY_AGGRESSIVE] = 32;
    // init GHB_lookahead & GHB_degree update action
    counter = 3;
    counter_update[ACCURACY_HIGH][LATE] = 1;
    counter_update[ACCURACY_HIGH][NOT_LATE] = 0;
    counter_update[ACCURACY_MEDIUM][LATE] = 1;
    counter_update[ACCURACY_MEDIUM][NOT_LATE] = 0;
    counter_update[ACCURACY_LOW][LATE] = -1;
    counter_update[ACCURACY_LOW][NOT_LATE] = 0;
}

uint32_t CACHE::l2c_prefetcher_operate(uint64_t addr, uint64_t ip, uint8_t cache_hit, uint8_t type, uint32_t metadata_in) {
    if ((ooo_cpu[cpu].num_retired % SAMPLE_INTERVAL) == 0 && pf_issued > 0 && pf_useful > 0) {
        // accuracy state
        double pf_accuracy = pf_useful / (double)pf_issued;
        accuracy_state_t accuracy_state;
        if (pf_accuracy > ACCURACY_THRESHOLD_H)
            accuracy_state = ACCURACY_HIGH;
        else if (pf_accuracy < ACCURACY_THRESHOLD_L)
            accuracy_state = ACCURACY_LOW;
        else
            accuracy_state = ACCURACY_MEDIUM;

        // lateness state
        double pf_lateness = pf_late / (double)pf_useful;
        lateness_state_t lateness_state;
        if (pf_lateness > LATENESS_THRESHOLD)
            lateness_state = LATE;
        else
            lateness_state = NOT_LATE;

        // update counter based on accuracy and lateness
        counter = (counter + counter_update[accuracy_state][lateness_state]) % (COUNTER_MAX_VAL + 1);
        if (counter < COUNTER_MIN_VAL)
            counter = COUNTER_MAX_VAL;
        assert(counter >= COUNTER_MIN_VAL && counter <= COUNTER_MAX_VAL);

        // update prefetcher
        GHB_lookahead = GHB_lookahead_table[counter - 1];
        GHB_degree = GHB_degree_table[counter - 1];
    }

    int32_t i;

    uint64_t cl_addr = addr >> LOG2_BLOCK_SIZE;
    int IT_index = ip % IT_SIZE;
    // set GHB entry
    GHB[cur_index].cl_addr = cl_addr;
    GHB[cur_index].prev = IT[IT_index];
    // update IT entry
    IT[IT_index] = cur_index;
    // get last_three_cl_addr
    int16_t index = cur_index;
    uint64_t last_three_cl_addr[3];
    cur_index = (cur_index + 1) % GHB_SIZE;

    last_three_cl_addr[0] = GHB[index].cl_addr;
    for (i = 1; i < 3; i++) {
        index = GHB[index].prev;
        if (index == INVALID_INDEX)
            return metadata_in;
        last_three_cl_addr[i] = GHB[index].cl_addr;
    }
    // get strides
    int64_t stride1 = 0;
    if (last_three_cl_addr[0] >= last_three_cl_addr[1])
        stride1 = last_three_cl_addr[0] - last_three_cl_addr[1];
    else {
        stride1 = last_three_cl_addr[1] - last_three_cl_addr[0];
        stride1 *= -1;
    }
    int64_t stride2 = 0;
    if (last_three_cl_addr[1] >= last_three_cl_addr[2])
        stride2 = last_three_cl_addr[1] - last_three_cl_addr[2];
    else {
        stride2 = last_three_cl_addr[2] - last_three_cl_addr[1];
        stride2 *= -1;
    }
    // Prefetch if the strides are equal
    if (stride1 == stride2) {
        uint64_t pf_addr;
        for (i = GHB_lookahead; i <= GHB_lookahead + GHB_degree; i++) {
            pf_addr = (cl_addr + i * stride1) << LOG2_BLOCK_SIZE;
            // only issue a prefetch if the prefetch address is in the same 4 KB page
            // as the current demand access address
            if ((pf_addr >> LOG2_PAGE_SIZE) != (addr >> LOG2_PAGE_SIZE))
                break;
            // check the MSHR occupancy to decide if we're going to prefetch to the L2 or LLC
            if (MSHR.occupancy < (MSHR.SIZE >> 1))
                prefetch_line(ip, addr, pf_addr, FILL_L2, 0);
            else
                prefetch_line(ip, addr, pf_addr, FILL_LLC, 0);
        }
    }
    return metadata_in;
}

uint32_t CACHE::l2c_prefetcher_cache_fill(uint64_t addr, uint32_t set, uint32_t way, uint8_t prefetch, uint64_t evicted_addr, uint32_t metadata_in) {
    return metadata_in;
}

void CACHE::l2c_prefetcher_final_stats() {
    std::cout << "CPU " << cpu << " L2C GHB prefetcher final stats" << std::endl;
}