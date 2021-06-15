package eo.backend.eolang.transformations

import eo.core.ast.{ EOAliasMeta, EOMeta, EOMetas, EORTMeta }

object Metas {
  def aliasMetaToEO(aliasMeta: EOAliasMeta): String =
    s"+alias ${aliasMeta.alias} ${aliasMeta.src}"

  def rtMetaToEO(rtMeta: EORTMeta): String =
    s"+rt ${rtMeta.rtName} ${rtMeta.src}"

  def metaToEO: EOMeta => String = {
    case am@EOAliasMeta(_, _) => aliasMetaToEO(am)
    case rtm@EORTMeta(_, _) => rtMetaToEO(rtm)
  }

  def metasToEO(metas: EOMetas): Iterable[String] = {
    val packCompiled = metas.pack.map(p => s"+package ${p}")
    val metasCompiled = metas.metas.map(metaToEO)

    packCompiled ++ List("") ++ metasCompiled
  }
}
