import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast._

private val mutualRecursionExample = EOProg(
  EOMetas(
    pack = Some("sandbox"),
    metas = Vector(
      EOAliasMeta("stdout", "org.eolang.io.stdout"),
      EOAliasMeta("sprintf", "org.eolang.txt.sprintf"),
    )
  ),
  Vector(
    EOBndExpr(
      EOAnyNameBnd(LazyBnd("base")),
      EOObj(
        freeAttrs = Vector(),
        varargAttr = None,
        bndAttrs = Vector(
          EOBndExpr(
            EOAnyNameBnd(LazyBnd("x")),
            EOSimpleApp("memory")
          ),
          EOBndExpr(
            EOAnyNameBnd(LazyBnd("f")),
            EOObj(
              freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
              varargAttr = None,
              bndAttrs = Vector(
                EOBndExpr(
                  EODecoration(),
                  EOCopy(
                    EODot(EOSimpleApp("x"), "write"),
                    NonEmpty[Vector[EOBnd]]( EOAnonExpr(EOSimpleApp("v")) )
                  )
                )
              )
            )
          ),
          EOBndExpr(
            EOAnyNameBnd(LazyBnd("g")),
            EOObj(
              freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
              varargAttr = None,
              bndAttrs = Vector(
                EOBndExpr(
                  EODecoration(),
                  EOCopy(
                    EODot(EOSimpleApp("self"), "f"),
                    NonEmpty[Vector[EOBnd]]( EOAnonExpr(EOSimpleApp("self")), EOAnonExpr(EOSimpleApp("v")) )
                  )
                )
              )
            )
          )
        )
      )
    ),


    EOBndExpr(
      EOAnyNameBnd(LazyBnd("derived")),
      EOObj(
        freeAttrs = Vector(),
        varargAttr = None,
        bndAttrs = Vector(
          EOBndExpr(EODecoration(), EOSimpleApp("base")),
          EOBndExpr(
            EOAnyNameBnd(LazyBnd("f")),
            EOObj(
              freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
              varargAttr = None,
              bndAttrs = Vector(
                EOBndExpr(
                  EODecoration(),
                  EOCopy(
                    EODot(EOSimpleApp("self"), "g"),
                    NonEmpty[Vector[EOBnd]]( EOAnonExpr(EOSimpleApp("self")), EOAnonExpr(EOSimpleApp("v")) )
                  )
                )
              ),
            )
          )
        )
      )
    ),
  )
)

def test(): Unit = {

}