/*  Title:      AI/src/General/json_schema.scala
    Author:     Kevin Kappelmann

JSON Schema with typed formats.
*/

package isabelle.pide.ai

import isabelle._

import scala.collection.immutable.VectorMap

object JSON_Schema {
  object Format {
    def apply[A](
      schema: JSON.Object.T,
      unapply_body: JSON.T => Option[A],
      apply_body: A => JSON.T
    ): Format[A] =
      new Format[A](schema) {
        def unapply(json: JSON.T): Option[A] = unapply_body(json)
        def apply(value: A): JSON.T = apply_body(value)
      }

    val string: Format[String] = Format(JSON_Object("type" -> "string"),
      JSON.Value.String.unapply, identity[String])
    val bool: Format[Boolean] = Format(JSON_Object("type" -> "boolean"),
      JSON.Value.Boolean.unapply, identity[Boolean])
    val obj: Format[JSON.Object.T] = Format(JSON_Object("type" -> "object"),
      JSON.Object.unapply, identity[JSON.Object.T])
    val any: Format[JSON.T] = Format(JSON.Object.empty, Some(_), identity[JSON.T])

    def enumeration[A](values: Iterable[A], name: A => String = (a: A) => a.toString): Format[A] =
      Format(JSON_Object("type" -> "string", "enum" -> values.map(name).toList),
        json => JSON.Value.String.unapply(json).flatMap(s => values.find(name(_) == s)), name)
    def int(
      minimum: Option[Int] = None,
      maximum: Option[Int] = None
    ): Format[Int] = {
      val unapply = (json: JSON.T) => JSON.Value.Int.unapply(json).filter(value =>
        minimum.forall(_ <= value) && maximum.forall(value <= _))
      Format(JSON_Object("type" -> "integer") ++
          JSON.optional("minimum" -> minimum) ++ JSON.optional("maximum" -> maximum),
        unapply, identity[Int])
    }

    val long: Format[Long] = Format(JSON_Object("type" -> "integer"),
      JSON.Value.Long.unapply, identity[Long])

    def nullable[A](format: Format[A]): Format[A | Null] =
      Format(JSON_Object("anyOf" -> List(format.schema, JSON_Object("type" -> "null"))),
        json => if (json == null) Some(null) else format.unapply(json),
        value => if (value == null) null else format(value.nn))

    def list[A](format: Format[A]): Format[List[A]] =
      Format(JSON_Object("type" -> "array", "items" -> format.schema),
        JSON.Value.List.unapply(_, format.unapply), _.map(format.apply))

    def dictionary[A](format: Format[A]): Format[VectorMap[String, A]] = {
      val unapply = (json: JSON.T) => JSON.Object.unapply(json).flatMap { obj =>
        val entries = obj.iterator.map { (name, value) =>
            format.unapply(value).map(result => name -> result)
          }.toList
        Option.when(entries.forall(_.isDefined))(VectorMap.from(entries.flatten))
      }
      val apply = (map: VectorMap[String, A]) =>
        JSON_Object(map.iterator.map { (name, value) => name -> format(value) })
      Format(JSON_Object("type" -> "object", "additionalProperties" -> format.schema),
        unapply, apply)
    }

    def property[A](name: String, format: Format[A]): Format[A] =
      Format(JSON_Object("type" -> "object", "properties" -> JSON_Object(name -> format.schema),
          "required" -> List(name), "additionalProperties" -> false),
        json => JSON.Object.unapply(json).filter(_.keySet == Set(name)).flatMap(obj =>
          format.unapply(obj(name))),
        value => JSON_Object(name -> format(value)))

    def result[A, E](res: Format[A], err: Format[E]): Format[Result[A, E]] =
      Format(JSON_Object("oneOf" -> List(res.schema, err.schema)),
        json => res.unapply(json).map(Result.Res(_)).orElse(err.unapply(json).map(Result.Error(_))),
        {
          case Result.Res(a) => res(a)
          case Result.Error(e) => err(e)
        })
  }

  abstract class Format[A] private(val schema: JSON.Object.T) {
    def apply(value: A): JSON.T
    def unapply(json: JSON.T): Option[A]

    def map[B](decode: A => B, encode: B => A): Format[B] =
      Format(schema, unapply(_).map(decode), b => apply(encode(b)))
  }

  object Arg {
    private def schema[A](
      format: Format[A],
      description: String,
      default: Option[A]
    ): JSON.Object.T =
      format.schema ++ JSON.optional("description" -> proper_string(description)) ++
      JSON.optional("default" -> default.map(format.apply))

    private def decode[A](name: String, format: Format[A])(json: JSON.T): A =
      format.unapply(json).getOrElse(
        throw new Arg_Error(List(name), "malformed " + JSON.Format(json)))

    private def missing(name: String): Nothing = throw new Arg_Error(List(name), "missing")

    final class Required[A] private[JSON_Schema](
      name: String,
      description: String,
      val format: Format[A]
    ) extends Arg[A](name) {
      def required: Boolean = true
      def schema: JSON.Object.T = Arg.schema(format, description, None)
      def get(obj: JSON.Object.T): A = obj.get(name).fold(missing(name))(decode(name, format))
      def apply(value: A): JSON.Object.T = JSON_Object(name -> format(value))
    }

    final class Optional[A] private[JSON_Schema](
      name: String,
      description: String,
      val format: Format[A]
    ) extends Arg[Option[A]](name) {
      def required: Boolean = false
      def schema: JSON.Object.T = Arg.schema(format, description, None)
      def get(obj: JSON.Object.T): Option[A] = obj.get(name).map(decode(name, format))
      def apply(value: Option[A]): JSON.Object.T =
        value.fold(JSON_Object())(v => JSON_Object(name -> format(v)))
    }

    final class Default[A] private[JSON_Schema](
      name: String,
      description: String,
      val format: Format[A],
      val default: A
    ) extends Arg[A](name) {
      def required: Boolean = false
      def schema: JSON.Object.T = Arg.schema(format, description, Some(default))
      def get(obj: JSON.Object.T): A = obj.get(name).fold(default)(decode(name, format))
      def apply(value: A): JSON.Object.T = JSON_Object(name -> format(value))
    }

    def required[A](name: String, description: String, format: Format[A]): Required[A] =
      new Required(name, description, format)

    def optional[A](name: String, description: String, format: Format[A]): Optional[A] =
      new Optional(name, description, format)

    def default[A](name: String, description: String, format: Format[A], default: A): Default[A] =
      new Default(name, description, format, default)
  }

  abstract class Arg[A] private[JSON_Schema](val name: String) {
    def required: Boolean
    def schema: JSON.Object.T
    def get(obj: JSON.Object.T): A
    def apply(value: A): JSON.Object.T

    def within[B](obj: JSON.Object.T)(body: A => B): B = {
      val value = get(obj)
      try body(value) catch { case exn: Arg_Error => throw exn.in(name) }
    }
  }

  final class Arg_Error private[JSON_Schema](path: List[String], reason: String)
    extends Exn.User_Error(quote(path.mkString(".")) + ": " + reason) {
    def in(name: String): Arg_Error = new Arg_Error(name :: path, reason)
  }

  def object_schema(args: List[Arg[?]], additional_properties: Boolean = false): JSON.Object.T =
    JSON_Object("type" -> "object",
      "properties" -> JSON_Object(args.map(arg => arg.name -> arg.schema)),
      "additionalProperties" -> additional_properties) ++
    JSON.optional("required" -> proper_list(args.filter(_.required).map(_.name)))

  def check_properties(schema: JSON.Object.T, obj: JSON.Object.T): Unit =
    if (schema.get("additionalProperties").contains(false)) {
      val known = JSON.Object.unapply(schema.getOrElse("properties", JSON.Object.empty))
        .fold(List.empty[String])(_.keys.toList)
      val unknown = obj.keys.filterNot(known.contains).toList.sorted
      if (unknown.nonEmpty) {
        error(s"Unknown argument(s) ${commas_quote(unknown)}. " +
          s"Valid argument(s): ${commas_quote(known)}")
      }
    }
}
