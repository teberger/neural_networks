function [ training_error, testing_error, weights ] = runEpoch_delta( p, train_samples, test_samples, eta )
%RUNEPOCH Summary of this function goes here
%   Detailed explanation goes here
    training_error = 0;
    weights = p;
    for i= 1 : length(train_samples(:,1))
        [new_weights, error] = learn_delta(weights, train_samples(i, :), eta, true);
        training_error = error + training_error;
        weights = new_weights;
    end

    testing_error = 0;
    for i= 1 : length(test_samples(:,1))
        [~, error] = learn_delta(p, test_samples(i, :), eta, false);
        testing_error = error + testing_error;
    end
    
end

