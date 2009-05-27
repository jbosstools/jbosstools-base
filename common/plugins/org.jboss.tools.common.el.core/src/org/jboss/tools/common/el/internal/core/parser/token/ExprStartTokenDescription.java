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

import org.jboss.tools.common.el.core.parser.Tokenizer;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ExprStartTokenDescription extends ConstantTokenDescription {
	public static final int EXPR_START = 13;

	public static ExprStartTokenDescription INSTANCE = new ExprStartTokenDescription();

	public ExprStartTokenDescription () {
		super("(", EXPR_START); //$NON-NLS-1$
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		boolean b = super.read(tokenizer, offset);
		if(b) {
			ParamUtil.openComplexExpressionContext(tokenizer.getContext());
		}
		return b;
	}

}
