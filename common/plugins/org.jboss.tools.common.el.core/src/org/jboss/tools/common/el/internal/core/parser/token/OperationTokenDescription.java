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

import org.jboss.tools.common.el.core.ElCoreMessages;
import org.jboss.tools.common.el.core.parser.ITokenDescription;
import org.jboss.tools.common.el.core.parser.Tokenizer;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class OperationTokenDescription implements ITokenDescription {
	public static final int OPERATION = 12;

	static String[] OPS = {
		"&&", "||", "==", "!=", "<=", ">=",  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
		"+", "-", "*", "/", "&", "%", "|", "?" , ":", "^", "<", ">", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$ //$NON-NLS-10$ //$NON-NLS-11$ //$NON-NLS-12$
	};
	private static final String[] OPS_2 = {
		"div", "and", "or", "not", "mod", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
		"eq", "ne", "lt", "gt", "ge", "le",  //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
	};

	public static OperationTokenDescription INSTANCE = new OperationTokenDescription();

	public String getName() {
		return ElCoreMessages.OperationTokenDescription_Name;
	}

	public int getType() {
		return OPERATION;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		int end = -1;
		boolean canBeFollowedByOperand = true;
		for (int i = 0; end < 0 && i < OPS.length; i++) {
			if(tokenizer.startsWith(OPS[i])) {
				end = offset + OPS[i].length();
			}
		}
		for (int i = 0; end < 0 && i < OPS_2.length; i++) {
			if(tokenizer.startsWith(OPS_2[i])) {
				end = offset + OPS_2[i].length();
				canBeFollowedByOperand = false;
			}
		}
		if(end < 0) return false;
		char ch = tokenizer.lookUpChar(end);
		if(Character.isWhitespace(ch) || ch == '\0' || !Character.isJavaIdentifierPart(ch)
			|| (canBeFollowedByOperand && Character.isJavaIdentifierPart(ch) 
					|| ch == '\'' || ch == '"' || ch == '-')) {
			return true;
		}
		
		return false;
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		if(!isStart(tokenizer, offset)) {
			return false;
		}
		int end = -1;
		for (int i = 0; end < 0 && i < OPS.length; i++) {
			if(tokenizer.startsWith(OPS[i])) {
				end = offset + OPS[i].length();
			}
		}
		for (int i = 0; end < 0 && i < OPS_2.length; i++) {
			if(tokenizer.startsWith(OPS_2[i])) {
				end = offset + OPS_2[i].length();
			}
		}
		tokenizer.addToken(OPERATION, offset, end);
		return true;
	}

}
