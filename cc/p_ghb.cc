#include "cache.h"

/*******************CONFIG*********************/

#define GHB_LOOKAHEAD 1
#define GHB_DEGREE 10

/*******************CONFIG*********************/

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
}

uint32_t CACHE::l2c_prefetcher_operate(uint64_t addr, uint64_t ip, uint8_t cache_hit, uint8_t type, uint32_t metadata_in) {
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
        for (i = GHB_LOOKAHEAD; i <= GHB_LOOKAHEAD + GHB_DEGREE; i++) {
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