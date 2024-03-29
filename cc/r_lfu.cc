#include "cache.h"

uint32_t lfu[LLC_SET][LLC_WAY];

// initialize replacement state
void CACHE::llc_initialize_replacement() {
    cout << "Initialize LFU state" << endl;
    uint32_t i, j;
    for (i = 0; i < LLC_SET; i++)
        for (j = 0; j < LLC_WAY; j++)
            lfu[i][j] = 0;
}

// find replacement victim
uint32_t CACHE::llc_find_victim(uint32_t cpu, uint64_t instr_id, uint32_t set, const BLOCK* current_set, uint64_t ip, uint64_t full_addr, uint32_t type) {
    uint32_t way = 0;

    // fill invalid line first
    for (way = 0; way < LLC_WAY; way++) {
        if (block[set][way].valid == false) {
            DP(if (warmup_complete[cpu]) {
            cout << "[" << NAME << "] " << __func__ << " instr_id: " << instr_id << " invalid set: " << set << " way: " << way;
            cout << hex << " address: " << (full_addr >> LOG2_BLOCK_SIZE) << " victim address: " << block[set][way].address << " data: " << block[set][way].data;
            cout << dec << " lfu: " << lfu[set][way] << endl; });

            break;
        }
    }

    // LFU victim
    if (way == LLC_WAY) {
        uint32_t min_freq = UINT32_MAX;
        for (uint32_t i = 0; i < LLC_WAY; i++) {
            if (lfu[set][i] < min_freq) {
                way = i;
                min_freq = lfu[set][i];
            }
        }
    }

    DP(if (warmup_complete[cpu]) {
    cout << "[" << NAME << "] " << __func__ << " instr_id: " << instr_id << " replace set: " << set << " way: " << way;
    cout << hex << " address: " << (full_addr>>LOG2_BLOCK_SIZE) << " victim address: " << block[set][way].address << " data: " << block[set][way].data;
    cout << dec << " lfu: " << lfu[set][way] << endl; });

    if (way == LLC_WAY) {
        cerr << "[" << NAME << "] " << __func__ << " no victim! set: " << set << endl;
        assert(0);
    }

    return way;
}

// called on every cache hit and cache fill
void CACHE::llc_update_replacement_state(uint32_t cpu, uint32_t set, uint32_t way, uint64_t full_addr, uint64_t ip, uint64_t victim_addr, uint32_t type, uint8_t hit) {
    string TYPE_NAME;
    if (type == LOAD)
        TYPE_NAME = "LOAD";
    else if (type == RFO)
        TYPE_NAME = "RFO";
    else if (type == PREFETCH)
        TYPE_NAME = "PF";
    else if (type == WRITEBACK)
        TYPE_NAME = "WB";
    else
        assert(0);

    if (hit)
        TYPE_NAME += "_HIT";
    else
        TYPE_NAME += "_MISS";

    if ((type == WRITEBACK) && ip)
        assert(0);

    // uncomment this line to see the LLC accesses
    // cout << "CPU: " << cpu << "  LLC " << setw(9) << TYPE_NAME << " set: " << setw(5) << set << " way: " << setw(2) << way;
    // cout << hex << " paddr: " << setw(12) << paddr << " ip: " << setw(8) << ip << " victim_addr: " << victim_addr << dec << endl;

    // update lfu
    if (hit) {
        lfu[set][way]++;
    } else {
        lfu[set][way] = 1;
    }
}

void CACHE::llc_replacement_final_stats() {
}