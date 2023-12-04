#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <unordered_set>
#include <numeric>

using namespace std;

#define IS_DIGIT(c) (c >= '0' && c <= '9')

struct card {
    int idx;
    int winners = 0;
    unordered_set<int> winning;
    unordered_set<int> nums;
};

size_t parseNum(string s, int* num) {
    int n = 0;
    size_t i = 0;
    while (IS_DIGIT(s[i])) {
        n *= 10;
        n += s[i] - '0';
        i++;
    }
    *num = n;
    return i;
}

card parseCard(string s, int id) {
    card c;
    c.idx = id;
    int idx, n;
    size_t i = s.find(":") + 1;
    bool isWinning = true;

    while (i < s.size()) {
        if (s[i] == '|') {
            isWinning = false;
            i += 2;
        } else if (IS_DIGIT(s[i])) {
            i += parseNum(s.substr(i), &n);
            if (isWinning)
                c.winning.insert(n);
            else
                c.nums.insert(n);
        } else {
            i++;
        }
    }

    return c;
}

double part1(vector<card>& cards) {
    double sum = 0;
    
    for (card& c : cards) {
        int n = 0;
        for (int i : c.nums) {
            if (c.winning.find(i) != c.winning.end())
                c.winners++;
        }
        if (c.winners > 0)
            sum += pow(2, c.winners-1);
    }

    return sum;
}

int part2(vector<card>& cards) {
    vector<int> copies(cards.size(), 1);
    for (card& c : cards) {
        int ncopies = copies[c.idx-1];
        for (int i = c.idx; i < c.idx + c.winners && i < cards.size(); i++) {
            copies[i] += ncopies;
        }
    }

    return accumulate(copies.begin(), copies.end(), 0);
}

int main(int argc, char** argv) {
    ifstream input("input.txt");
    string s;
    vector<card> cards;
    int id = 1;

    while (getline(input, s)) {
        cards.push_back(parseCard(s, id++));
    }

    cout << "Part 1: " << part1(cards) << endl;
    cout << "Part 2: " << part2(cards) << endl;
}