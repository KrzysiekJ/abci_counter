# ABCI counter

An example application using [`abci_server`](https://github.com/KrzysiekJ/abci_server), similar to [the original Tendermint’s counter application](https://github.com/tendermint/abci/blob/eaeb26/example/counter/counter.go).

The application’s state is a count of valid transactions that got into chain, represented as a big endian binary. A transaction passes `DeliverTX` if it is equal to the current counter. A transaction passes `CheckTx` if it is greater than or equal to the current counter.

[erlang.mk](https://erlang.mk/) is used as a build tool.

## Usage

You need [Erlang](https://www.erlang.org/) installed. To build a release and start the server, execute:

```
make run
```

## License

This software is licensed under under [the Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the “License”); you may not use this software except in compliance with the License. Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the License for the specific language governing permissions and limitations under the License.
