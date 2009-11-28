/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import java.util.List;

import org.eclipse.jface.text.Region;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserFactory;

/**
 * Represents "var"/"value" attributes.
 * @author Alexey Kazakov
 */
public class Var {
	String name;
	String value;
	ELExpression elToken;
	String resolvedValue;
	ELExpression resolvedElToken;
	int declOffset;
	int declLength;
	ELParserFactory factory;
	Region region;

	/**
	 * Constructor
	 * @param name - value of "var" attribute. 
	 * @param value - value of "value" attribute.
	 */
	public Var(ELParserFactory factory, String name, String value, int declOffset, int declLength) {
		this.factory = factory;
		this.name = name;
		this.value = value;
		elToken = parseEl(value);
		this.declOffset = declOffset;
		this.declLength = declLength;
	}

	ELExpression parseEl(String el) {
		if(el.length()>3 && el.startsWith("#{") && el.endsWith("}")) { //$NON-NLS-1$ //$NON-NLS-2$
			ELParser parser = factory.createParser();
			ELModel model = parser.parse(el);
			if(model == null || model.getSyntaxErrors().size() > 0) return null;
			List<ELInstance> is = model.getInstances();
			if(is.size() == 0) return null;
			return is.get(0).getExpression();
		}
		return null;
	}

	/**
	 * Sets value to new resolved EL which we got as result of parsing value.
	 * For example:
	 * <h:datatable value="#{list}" var="item">
	 * 	<h:dataTable value="#{item.anotherList}" var="innerItem">
	 * 		...
	 * 	</h:dataTable>
	 * </h:dataTable>
	 * Original El is #{item.anotherList}
	 * Resolved El is #{list.iterator().next().anotherList}
	 * It's very useful for nested vars.
	 * @param newEl
	 */
	public void resolveValue(String newEl) {
		resolvedValue = newEl;
		resolvedElToken = parseEl(newEl);
	}

	/**
	 * @return parsed EL from "value" attribute. Returns null if EL is not valid.
	 */
	public ELExpression getElToken() {
		return elToken;
	}

	/**
	 * @return parsed resolved EL from "value" attribute. May be null.
	 */
	public ELExpression getResolvedElToken() {
		return resolvedElToken;
	}

	/**
	 * @return name of variable. 
	 */
	public String getName() {
		return name;
	}

	/**
	 * @return value of variable. It's EL.
	 */
	public String getValue() {
		return value;
	}

	/**
	 * @return resolved value of variable. It's EL. May be null.
	 */
	public String getResolvedValue() {
		return resolvedValue;
	}

	/**
	 * @return offset of the var declaration
	 */
	public int getDeclarationOffset() {
		return declOffset;
	}

	/**
	 * @return length of the var declaration
	 */
	public int getDeclarationLength() {
		return declLength;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof Var) {
			Var compare = (Var)obj;
			String str = getName();
			if (str != null) {
				if (!str.equals(compare.getName()))
					return false;
			} else {
				if (compare.getName() != null)
					return false;
			}
			str = getValue();
			return (str != null ?
				str.equals(compare.getValue()) :
				compare.getValue() == null);
		}
		return false;
	}

	/**
	 * @return the region
	 */
	public Region getRegion() {
		return region;
	}

	/**
	 * @param region the region to set
	 */
	public void setRegion(Region region) {
		this.region = region;
	}
}