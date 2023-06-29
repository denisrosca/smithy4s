/*
 *  Copyright 2021-2022 Disney Streaming
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
package http4s
package internals

import smithy4s.http.HttpDiscriminator
import smithy4s.http4s.kernel._
import smithy4s.schema.CachedSchemaCompiler
import org.http4s.Response
import org.http4s.headers.`Content-Type`
import cats.effect.Concurrent
import smithy4s.http.Metadata
import org.http4s.syntax.all._

private[http4s] class SimpleRestJsonCodecs(
    val maxArity: Int,
    val explicitNullEncoding: Boolean
) extends SimpleProtocolCodecs {
  private val hintMask =
    alloy.SimpleRestJson.protocol.hintMask ++ HintMask(IntEnum)
  private val underlyingCodecs = smithy4s.http.json.codecs(hintMask, maxArity)
  private val errorHeaders = List(
    smithy4s.http.errorTypeHeader,
    // Adding X-Amzn-Errortype as well to facilitate interop
    // with Amazon-issued code-generators.
    smithy4s.http.amazonErrorTypeHeader
  )

  private def addEmptyJsonToResponse[F[_]](
      response: Response[F]
  ): Response[F] = {
    response
      .withBodyStream(
        response.body
          .ifEmpty(fs2.Stream.chunk(fs2.Chunk.array("{}".getBytes())))
      )
      .withContentType(`Content-Type`(mediaType"application/json"))
  }

  def makeServerCodecs[F[_]: Concurrent]: UnaryServerCodecs.Make[F] = {
    val messageDecoderCompiler =
      RequestDecoder.restSchemaCompiler[F](
        Metadata.Decoder,
        EntityDecoders.fromCodecAPI[F](underlyingCodecs)
      )
    val responseEncoderCompiler = {
      val restSchemaCompiler = ResponseEncoder.restSchemaCompiler[F](
        Metadata.Encoder,
        EntityEncoders.fromCodecAPI[F](underlyingCodecs)
      )
      new CachedSchemaCompiler[ResponseEncoder[F, *]] {
        type Cache = restSchemaCompiler.Cache
        def createCache(): Cache = restSchemaCompiler.createCache()
        def fromSchema[A](schema: Schema[A]) = fromSchema(schema, createCache())

        def fromSchema[A](schema: Schema[A], cache: Cache) = if (
          schema.isUnit
        ) {
          restSchemaCompiler.fromSchema(schema, cache)
        } else {
          restSchemaCompiler
            .fromSchema(schema, cache)
            .andThen(addEmptyJsonToResponse(_))
        }
      }
    }

    UnaryServerCodecs.make[F](
      input = messageDecoderCompiler,
      output = responseEncoderCompiler,
      error = responseEncoderCompiler,
      errorHeaders = errorHeaders
    )
  }

  def makeClientCodecs[F[_]: Concurrent]: UnaryClientCodecs.Make[F] = {
    val messageDecoderCompiler =
      ResponseDecoder.restSchemaCompiler[F](
        Metadata.Decoder,
        EntityDecoders.fromCodecAPI[F](underlyingCodecs)
      )
    val messageEncoderCompiler =
      RequestEncoder.restSchemaCompiler[F](
        Metadata.Encoder,
        EntityEncoders.fromCodecAPI[F](underlyingCodecs)
      )
    UnaryClientCodecs.Make[F](
      input = messageEncoderCompiler,
      output = messageDecoderCompiler,
      error = messageDecoderCompiler,
      response =>
        Concurrent[F].pure(
          HttpDiscriminator.fromMetadata(
            errorHeaders,
            getResponseMetadata(response)
          )
        )
    )
  }

}