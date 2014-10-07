function [ new_weights, error ] = learn_delta( weights, input, eta , shouldLearn)
%LEARN_DELTA Summary of this function goes here
%   Detailed explanation goes here
    x = weights * input(1:2)';
    d = input(3);

    error = d - x;
    if (shouldLearn)
        new_weights = weights + (error * eta * input(1:2));
    else
        new_weights = weights;
    end 
    error = (error ^ 2)/2;
end