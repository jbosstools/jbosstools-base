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
public class UnaryTokenDescription extends ConstantTokenDescription {
	public static final int UNARY = 15;

	public static UnaryTokenDescription INSTANCE = new UnaryTokenDescription();

	private static final String[] OPS_2 = {
		"not", "empty"  //$NON-NLS-1$//$NON-NLS-2$
	};

	public UnaryTokenDescription() {
		super("!", UNARY); //$NON-NLS-1$
		addContent("--"); //$NON-NLS-1$
		addContent("++"); //$NON-NLS-1$
		addContent("+"); //$NON-NLS-1$
		addContent("-"); //$NON-NLS-1$
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		if(super.isStart(tokenizer, offset)) {
			return true;
		}
		int end = -1;
		for (int i = 0; end < 0 && i < OPS_2.length; i++) {
			if(tokenizer.startsWith(OPS_2[i])) {
				end = offset + OPS_2[i].length();
			}
		}
		if(end < 0) return false;
		char ch = tokenizer.lookUpChar(end);
		if(Character.isWhitespace(ch) || ch == '\0' || ch == '(' || !Character.isJavaIdentifierPart(ch)
			) {
			return true;
		}

		return false;
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		if(super.isStart(tokenizer, offset)) {
			return super.read(tokenizer, offset);
		}
		int end = -1;
		for (int i = 0; end < 0 && i < OPS_2.length; i++) {
			if(tokenizer.startsWith(OPS_2[i])) {
				end = offset + OPS_2[i].length();
			}
		}
		tokenizer.addToken(getType(), offset, end);
		return true;
	}

}
