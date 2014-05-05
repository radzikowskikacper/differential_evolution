/*
 * utils.hpp
 *
 *  Created on: 25 kwi 2014
 *      Author: michal
 */

#include <iostream>
#include "../header/bitmap_image.hpp"

class MyException
{
	std::string problem;
public:
	MyException():problem("")
	{
	}
	MyException(std::string problem):problem(problem){
	}
	std::string what()
	{
		return problem;
	}
	~MyException(){}
};

namespace utils
{
	bool parse_filename(std::string& filename);

	double goldstein(double x, double y);

	void help();

	bitmap_image* generatePointsToBmp(int points_per_unit,
			double (*fun_calc)(double x, double y),
			double x_left, double x_right,
			double y_top, double y_bottom,
			int count_isometric , int percent_isometric_drawing = 100);

}