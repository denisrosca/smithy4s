/*
 *  Copyright 2021 Disney Streaming
 *
 *  Licensed under the Tomorrow Open Source Technology License, Version 1.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *     https://disneystreaming.github.io/TOST-1.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package smithy4s

sealed trait StreamingSchema[A]

object StreamingSchema {
  def apply[A](fieldName: String, schema: Schema[A]): StreamingSchema[A] =
    Streamed(fieldName, schema)
  def nothing: StreamingSchema[Nothing] = NoStream

  case class Streamed[A](fieldName: String, schema: Schema[A])
      extends StreamingSchema[A]
  case object NoStream extends StreamingSchema[Nothing]
}