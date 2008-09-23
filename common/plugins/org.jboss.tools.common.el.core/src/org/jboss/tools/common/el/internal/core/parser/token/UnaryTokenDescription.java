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
package org.jboss.tools.common.el.internal.core.parser.token;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class UnaryTokenDescription extends ConstantTokenDescription {
	public static final int UNARY = 15;

	public static UnaryTokenDescription INSTANCE = new UnaryTokenDescription();

	public UnaryTokenDescription() {
		super("!", UNARY);
		addContent("--");
		addContent("++");
		addContent("+");
		addContent("-");
	}

}
