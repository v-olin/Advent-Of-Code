#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>

enum type {
    FIVES = 6,
    FOURS = 5,
    HOUSE = 4,
    THREES = 3,
    TWOPAIR = 2, 
    PAIR = 1,
    HIGH = 0
};

std::unordered_map<char, int> values = {
    {'J', 0},
    {'2', 1},
    {'3', 2},
    {'4', 3},
    {'5', 4},
    {'6', 5},
    {'7', 6},
    {'8', 7},
    {'9', 8},
    {'T', 9},
    {'J', 10},
    {'Q', 11},
    {'K', 12},
    {'A', 13}
};

struct hand {
    std::string in;
    std::unordered_map<char, int> cards;
    unsigned int idx;
    type rank;
    int bid;
};

struct hand_less_than {
    inline bool operator() (const hand& h1, const hand& h2) {
        if (h1.rank == h2.rank) {
            for (int i = 0; i < 5; i++) {
                if (values[h1.cards[i]] != values[h2.cards[i]])
                    return values[h1.cards[i]] < values[h2.cards[i]];
            }
        }
        
        return h1.rank < h2.rank;
    }
};

std::unordered_map<int, std::string> m = {
    {0, "HIGH"},
    {1, "PAIR"},
    {2, "TWOPAIR"},
    {3, "THREES"},
    {4, "HOUSE"},
    {5, "FOURS"},
    {6, "FIVES"}
};

type getType(hand h) {
    for (char c : h.in) {
        if (h.cards.find(c) == h.cards.end())
            h.cards[c] = 1;
        else
            h.cards[c]++;
    }

    if (h.cards.size() == 5)
        return HIGH;
    else if (h.cards.size() == 4)
        return PAIR;
    else if (h.cards.size() == 3) {
        for (auto it = h.cards.begin(); it != h.cards.end(); it++) {
            if (it->second == 3)
                return THREES;
            else if (it->second == 2)
                return TWOPAIR;
        }
    } else if (h.cards.size() == 2) {
        for (auto it = h.cards.begin(); it != h.cards.end(); it++) {
            if (it->second == 4)
                return FOURS;
            else if (it->second == 3)
                return HOUSE;
        }
    }
    
    return FIVES;
}

hand parse(std::string s, unsigned int idx) {
    return hand{
        s.substr(0, 5),
        std::unordered_map<char, int>(),
        idx,
        getType(s.substr(0, 5)),
        std::stoi(s.substr(6))
    };
}

int part1(std::vector<hand>& hands) {
    std::vector<hand> hp(hands);
    std::sort(hp.begin(), hp.end(), hand_less_than());
    unsigned long sum = 0;
    for (int i = 0; i < hp.size(); i++)
        sum += hp[i].bid * (i + 1);
    return sum;
}

type bestPossible(hand h) {
    if (h.cards.find('J') == h.cards.end())
        return h.rank;

    int maxOccs = 0;

    for (auto& it : h.cards) {
        if (it.second > maxOccs)
            maxOccs = it.second;
    }

    if (maxOccs == 4)
        return FIVES;
    else if (maxOccs == 3)
        return THREES;
    else if (maxOccs == 2)
        return PAIR;
    else
        return HIGH;
}

int part2(std::vector<hand>& hands) {


    return 0;
}

int main(int argc, char** argv) {

    if (argc != 2) {
        std::cout << "No input file" << std::endl;
        return 1;
    }

    std::ifstream input(argv[1]);
    std::string line;
    std::vector<hand> lines;
    unsigned int idx = 0;

    while (getline(input, line)) {
        lines.push_back(parse(line, idx));
    }

    std::cout << "Part 1: " << part1(lines) << std::endl;
    // std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}