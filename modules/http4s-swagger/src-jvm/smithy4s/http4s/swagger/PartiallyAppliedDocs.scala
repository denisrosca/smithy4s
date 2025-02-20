/*
 *  Copyright 2021-2023 Disney Streaming
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
package http4s.swagger

import cats.effect.Sync
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.StaticFile
import cats.data.NonEmptyList

case class PartiallyAppliedDocs[F[_]](path: String, swaggerUiPath: String) {
  def apply(
      first: HasId,
      rest: HasId*
  )(implicit
      F: Sync[F]
  ): HttpRoutes[F] = {
    val docs =
      new Docs[F](NonEmptyList(first, rest.toList), path, swaggerUiPath) {
        override def staticResource(
            name: String,
            req: Option[Request[F]]
        ) = {
          StaticFile.fromResource(name, req)
        }
      }
    docs.routes
  }

  def withPath(path: String): PartiallyAppliedDocs[F] =
    this.copy(path = path)

  def withSwaggerUiResources(
      swaggerUiPath: String
  ): PartiallyAppliedDocs[F] =
    this.copy(swaggerUiPath = swaggerUiPath)
}
