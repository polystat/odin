<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                id="simplify-xmir"
>
    <xsl:template match="o[@base and starts-with(@base, '.')]">
        <copy>
            <xsl:attribute name="bound-to">
                <xsl:value-of select="@name"/>
            </xsl:attribute>
            <of>
                <attribute>
                    <xsl:attribute name="name">
                        <xsl:value-of select="substring-after(@base, '.')"/>
                    </xsl:attribute>
                    <of>
                        <xsl:apply-templates select="*[position() = 1]"/>
                    </of>
                </attribute>
            </of>
            <with>
                <xsl:apply-templates select="*[position() > 1]"/>
            </with>
        </copy>
    </xsl:template>
    <xsl:template match="o[@base and not(starts-with(@base, '.'))]">
        <copy>
            <xsl:attribute name="bound-to">
                <xsl:value-of select="@name"/>
            </xsl:attribute>
            <of>
                <simple-app>
                    <xsl:attribute name="name">
                        <xsl:value-of select="@base"/>
                    </xsl:attribute>
                </simple-app>
            </of>
            <with>
                <xsl:apply-templates select="*"/>
            </with>
        </copy>
    </xsl:template>
    <xsl:template match="o[not(@base)]">
        <abstraction>
            <xsl:attribute name="bound-to">
                <xsl:value-of select="@name"/>
            </xsl:attribute>
            <xsl:apply-templates select="*"/>
        </abstraction>
    </xsl:template>
    <xsl:template match="o[not(@base) and @name and @line = parent::o/@line]">
        <free>
            <xsl:value-of select="@name"/>
        </free>
    </xsl:template>
    <xsl:template match="o[@data]">
        <data>
            <xsl:attribute name="type">
                <xsl:value-of select="@data"/>
            </xsl:attribute>
            <xsl:attribute name="value">
                <xsl:value-of select="text()"/>
            </xsl:attribute>
        </data>
    </xsl:template>
    <xsl:template match="node()|@*">
        <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>