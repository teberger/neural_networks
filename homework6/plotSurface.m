function [ ] = plotSurface( file )
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here

    figure;
    hold on;
    data = load(file);
    
    
    
    x = data(:, 2);
    y = data(:, 3);
    z = data(:, 1);
    
    tri = delaunay(x,y);
    
    h = trisurf(tri, x,y,z);
    axis vis3d
    
    lighting GOURAUD
    shading interp
    colorbar EastOutside
    
    hold off;
end

