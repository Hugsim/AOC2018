#include "pch.h"
#include <iostream>
#include <string>
#include <fstream>
#include <set>
#include <vector>

using namespace std;

class Claim {
public:
	int id;
	int xpos;
	int ypos;
	int width;
	int height;
};

Claim strToClaim(string s) {

}

int main()
{
	vector<Claim> claims;
	fstream input;
	string line;
	input.open("input.txt");
	while (getline(input, line))
	{
		claims.push_back(strToClaim(line));
	}



	input.close(); 
}

