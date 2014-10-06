function [ training_error, testing_error, new_weights ] = runEpoch( p, train_samples, test_samples, eta )
%RUNEPOCH Summary of this function goes here
%   Detailed explanation goes here
    training_error = 0;
    for i= 1 : length(train_samples(:,1))
        [new_weights, error] = learn(p, train_samples(i, :), eta, true);
        training_error = error + training_error;
        p = new_weights;
    end

    testing_error = 0;
    for i= 1 : length(test_samples(:,1))
        [~, error] = learn(p, test_samples(i, :), eta, false);
        testing_error = error + testing_error;
    end

end

