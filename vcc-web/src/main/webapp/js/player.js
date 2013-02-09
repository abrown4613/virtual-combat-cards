/*
 * Copyright (C) 2008-2013 - Thomas Santana <tms@exnebula.org>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */
function FetchCtrl($scope, $http) {
    $scope.method = 'GET';
    $scope.url = 'player-view';
    $scope.running = false;

    $scope.fetch = function () {
        $scope.code = null;
        $scope.response = null;
        $scope.running = true;

        $http({method: $scope.method, url: $scope.url, cache: false}).
            success(function (data, status) {
                $scope.status = status;
                $scope.data = data;
                $scope.fetch();
            }).
            error(function (data, status) {
                $scope.data = data || "Request failed";
                $scope.running = false;
                $scope.status = status;
            });
    };

    $scope.updateModel = function (method, url) {
        $scope.method = method;
        $scope.url = url;
    };
}