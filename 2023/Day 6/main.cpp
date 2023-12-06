#include <iostream>
#include <vector>
#include <stdint.h>

// Part 1
#define RACES {53, 313}, {89, 1090}, {76, 1213}, {98, 1201}
// Part 2
#define RACE {53897698, 313109012131201}

struct race {
    uint64_t time;
    uint64_t distance;
};

uint64_t part1(std::vector<race> races) {
    uint64_t prod = 1;

    for (auto& r : races) {
        uint64_t wins = 0;
        for (uint64_t i = 1; i <= (r.time - 1) / 2; i++) {
            if (i * (r.time - i) > r.distance)
                wins++;
        }
        prod *= 2 * wins + ((r.time & 1) ^ 1);
        wins = 0;
    }

    return prod;
}

uint64_t part2(race r) {
    uint64_t wins = 0;
    for (uint64_t i = 1; i <= (r.time - 1) / 2; i++) {
        if (i * (r.time - i) > r.distance)
            wins++;
    }

    return 2 * wins + ((r.time & 1) ^ 1);
}

int main() {

    std::vector<race> races = { RACES }; // part 1
    race r = RACE; // part 2

    std::cout << "Part 1: " << part1(races) << std::endl;
    std::cout << "Part 2: " << part2(r) << std::endl;

    return 0;
}