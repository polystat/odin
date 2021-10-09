<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                id="simplify-xmir"
>
    <xsl:template match="o[@base and starts-with(@base, '.')]">
        <copy>
            <xsl:if test="@name">
                <xsl:attribute name="bound-to">
                    <xsl:value-of select="@name"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="@const"/>
            <of>
                <attribute name="{substring-after(@base, '.')}">
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
            <xsl:if test="@name">
                <xsl:attribute name="bound-to">
                    <xsl:value-of select="@name"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="@const"/>
            <of>
                <simple-app name="{@base}"/>
            </of>
            <with>
                <xsl:apply-templates select="*"/>
            </with>
        </copy>
    </xsl:template>
    <xsl:template match="o[not(@base)]">
        <abstraction>
            <xsl:if test="@name">
                <xsl:attribute name="bound-to">
                    <xsl:value-of select="@name"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="@const"/>
            <xsl:apply-templates select="*"/>
        </abstraction>
    </xsl:template>
    <xsl:template match="o[not(@base) and @name and @line = parent::o/@line]">
        <free name="{@name}">
            <xsl:apply-templates select="@vararg"/>
        </free>
    </xsl:template>
    <xsl:template match="o[@data]">
        <data type="{@data}" value="{text()}">
            <xsl:if test="@name">
                <xsl:attribute name="bound-to">
                    <xsl:value-of select="@name"/>
                </xsl:attribute>
            </xsl:if>
            <xsl:apply-templates select="@const"/>
        </data>
    </xsl:template>
    <xsl:template match="node()|@*">
        <xsl:copy>
            <xsl:apply-templates select="node()|@*"/>
        </xsl:copy>
    </xsl:template>
</xsl:stylesheet>
