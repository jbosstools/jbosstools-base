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
		"&&", "||", "==", "!=", "<=", ">=", 
		"+", "-", "*", "/", "&", "%", "|", "?" , ":", "^", "<", ">",
	};
	private static final String[] OPS_2 = {
		"div", "and", "or", "not", "mod",
		"eq", "ne", "lt", "gt", "ge", "le", 
	};

	public static OperationTokenDescription INSTANCE = new OperationTokenDescription();

	public String getName() {
		return "Operator";
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
