#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <regex>

#define INPUT "test.txt"
#define max(a, b) (a > b ? a : b)

using namespace std;

struct hand {
    int r, g, b;
};

struct game {
    string s;
    size_t i = SIZE_MAX;
    vector<hand> hands;
};

void parseHands(string s, vector<hand>& hands) {
    regex r("([0-9]+ ([a-z]+))");
    int n;
    smatch m;
    hand h;

    if (regex_search(s, m, r)) {
        sregex_iterator i = sregex_iterator(s.begin(), s.end(), r);
        h = hand{ 0, 0, 0 };
        for (; i != sregex_iterator(); ++i) {
            m = *i;
            n = stoi(m.str());
            char c;
            for (size_t i = 2; i < m.str().length(); i++) {
                c = m.str()[i];
                if (c == 'r') { h.r += n; break; }
                else if (c == 'g') { h.g += n; break; }
                else if (c == 'b') { h.b += n; break; }
            }
        }
        hands.push_back(h);
    }
}

void parseGame(game& g) {
    regex r("((([0-9]+)\\s([a-z]+)((,\\s)*))(;*))+");
    smatch m;
    if (regex_search(g.s, m, r)) {
        sregex_iterator i = sregex_iterator(g.s.begin(), g.s.end(), r);
        for (; i != sregex_iterator(); ++i) {
            m = *i;
            parseHands(m.str(), g.hands);
        }
    }
}

int part1(vector<game>& games) {
    int sum = 0;

    for (auto& g : games) {
        bool possible = true;
        for (auto& h : g.hands) {
            if (h.r > 12 || h.g > 13 || h.b > 14) {
                possible = false;
            }
        }
        if (possible)
            sum += g.i;
    }

    return sum;
}

int part2(vector<game>& games) {
    int sum = 0;

    for (auto& game : games) {
        int r = 0, g = 0, b = 0;
        for (auto& h : game.hands) {
            r = max(r, h.r);
            g = max(g, h.g);
            b = max(b, h.b);
        }
        sum += r * g * b;
    }

    return sum;
}

int main() {
    ifstream in(INPUT);
    vector<game> lines;
    string s;
    size_t i = 1;

    while (getline(in, s)) {
        game g{ s, i++, vector<hand>() };
        parseGame(g);
        lines.push_back(g);
    }

    cout << "Part 1: " << part1(lines) << endl;
    cout << "Part 2: " << part2(lines) << endl;

    return 0;
}