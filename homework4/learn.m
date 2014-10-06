function [ new_weights, error ] = learn( weights, input, eta , shouldLearn)
%LEARN Summary of this function goes here
%   Detailed explanation goes here
    x = weights * input(1:2)';
    y = sign(x);
    d = input(3);

    error = sign(d - y);
    if (shouldLearn)
        new_weights = weights + (error * eta * input(1:2));
    else
        new_weights = weights;
    end 
    error = abs(error);
end