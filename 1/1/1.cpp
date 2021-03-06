#include "pch.h"
#include <iostream>
#include <fstream>
#include <string>
#include <set>
#include <vector>

using namespace std;

int ex1() {
	int state = 0;
	ifstream input;
	string line;
	input.open("input.txt");
	while (getline(input, line))
	{
		int change = stoi(line, nullptr);
		state += change;
	}
	return state;
	input.close();
}

int ex2() {
	vector<int> changes;
	set<int> visited;
	int state = 0;
	ifstream input;
	string line;
	input.open("input.txt");
	while (getline(input, line))
	{
		changes.push_back(stoi(line, nullptr));
	}

	int i = 0;

	while (true) {
		state += changes[i];

		if (visited.count(state) != 0) {
			break;
		}

		visited.insert(state);

		i = (i + 1) % changes.size();
	}

	return state;
}

int main()
{
	cout << ex1() << endl;
	cout << ex2() << endl;
}