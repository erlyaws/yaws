-record('soap:detail', {anyAttribs, choice}).
-record('soap:Fault', {anyAttribs, 'faultcode', 'faultstring', 'faultactor', 'detail'}).
-record('soap:Body', {anyAttribs, choice}).
-record('soap:Header', {anyAttribs, choice}).
-record('soap:Envelope', {anyAttribs, 'Header', 'Body', choice}).
-record('p:GetWeatherByPlaceNameResponse', {anyAttribs, 'GetWeatherByPlaceNameResult'}).
-record('p:GetWeatherByPlaceName', {anyAttribs, 'PlaceName'}).
-record('p:WeatherData', {anyAttribs, 'Day', 'WeatherImage', 'MaxTemperatureF',
                          'MinTemperatureF', 'MaxTemperatureC', 'MinTemperatureC'}).
-record('p:ArrayOfWeatherData', {anyAttribs, 'WeatherData'}).
-record('p:WeatherForecasts', {anyAttribs, 'Latitude', 'Longitude', 'AllocationFactor',
                               'FipsCode', 'PlaceName', 'StateCode', 'Status', 'Details'}).
-record('p:GetWeatherByZipCodeResponse', {anyAttribs, 'GetWeatherByZipCodeResult'}).
-record('p:GetWeatherByZipCode', {anyAttribs, 'ZipCode'}).
