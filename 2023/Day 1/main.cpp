#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

#define INPUT "input.txt"

struct data {
    int val = -1;
    size_t pos = SIZE_MAX;
};

struct line {
    std::string s;
    data fi, li, fs, ls;
};

bool isDigit(char c) {
    return c >= '0' && c <= '9';
}

void findDigits(line& l) {
    char *first = &(l.s.front());
    char *last = &(l.s.back());

    while (first <= last) {
        if (isDigit(*first) && l.fi.val == -1) {
            l.fi.val = *first - '0';
            l.fi.pos = first - &(l.s.front());
        }
        if (isDigit(*last) && l.li.val == -1) {
            l.li.val = *last - '0';
            l.li.pos = last - &(l.s.front());
        }

        if (l.fi.val != -1 && l.li.val != -1) {
            break;
        }

        if (l.fi.val == -1)
            first++;
        if (l.li.val == -1)
            last--;
    }

    if (first > last)
        l.li = l.fi;
}

void findStrs(line& l) {
    const std::vector<std::string> nums = { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine" };

    size_t len = l.s.length();
    std::string ts = l.s, tn;
    std::reverse(ts.begin(), ts.end());

    // very bad solution :/
    for (size_t i = 0; i < nums.size(); i++) {
        size_t pos = l.s.find(nums[i]);
        if (pos != std::string::npos) {
            if (l.fs.val == -1 || l.fs.pos > pos) {
                l.fs.val = i;
                l.fs.pos = pos;
            }
        }
        tn = nums[i];
        std::reverse(tn.begin(), tn.end());
        pos = ts.find(tn);
        if (pos != std::string::npos) {
            if (l.ls.val == -1 || l.ls.pos < len - pos) {
                l.ls.val = i;
                l.ls.pos = len - pos;
            }
        }
    }
}

line parse(std::string in) {
    line l;
    l.s = in;

    findDigits(l);
    findStrs(l);

    return l;
}

int part1(std::vector<line>& lines) {
    int sum = 0;

    for (auto& l : lines)
        sum += l.fi.val * 10 + l.li.val;

    return sum;
}

int part2(std::vector<line>& lines) {
    int sum = 0;
    int first, last;

    for (auto& l : lines)
    {
        if (l.fs.pos == SIZE_MAX)
            first = l.fi.val;
        else if (l.fi.pos == SIZE_MAX)
            first = l.fs.val;
        else
            first = l.fs.pos < l.fi.pos ? l.fs.val : l.fi.val;

        if (l.ls.pos == SIZE_MAX)
            last = l.li.val;
        else if (l.li.pos == SIZE_MAX)
            last = l.ls.val;
        else
            last = l.ls.pos > l.li.pos ? l.ls.val : l.li.val;

        sum += first * 10 + last;
    }

    return sum;
}

int main() {
    std::ifstream in(INPUT);
    std::string s;
    std::vector<line> lines;

    while (std::getline(in, s)) {
        lines.push_back(parse(s));
    }

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}