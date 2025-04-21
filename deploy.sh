#!/bin/bash
clear
echo "Cleaning up previous builds and generated files..."
cabal clean

echo "Deleting the database and generated folders..."
rm -rf database
rm -rf generated

echo "Building the project..."
cabal build all

echo "Running admin..."
cabal run admin

echo "Running dbbuilder..."
cabal run dbbuilder

echo "Running server..."
cabal run server

echo "Opening a new terminal to run query-admin..."
gnome-terminal -- bash -c "cabal run query-admin; exec bash"

echo "Deployment complete."
