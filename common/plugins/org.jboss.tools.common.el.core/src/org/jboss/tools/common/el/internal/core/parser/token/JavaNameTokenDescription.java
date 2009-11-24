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
public class JavaNameTokenDescription implements ITokenDescription {
	public static final int JAVA_NAME = 3;

	public static JavaNameTokenDescription INSTANCE = new JavaNameTokenDescription();
	
	public static ConstantTokenDescription INSTANCEOF_INSTANCE = InstanceofTokenDescription.INSTANCE;

	public String getName() {
		return ElCoreMessages.JavaNameTokenDescription_Name;
	}

	public int getType() {
		return JAVA_NAME;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		if(OperationTokenDescription.INSTANCE.isStart(tokenizer, offset)) {
			return false;
		}
		if(InstanceofTokenDescription.INSTANCE.isStart(tokenizer, offset)) {
			return false;
		}
		char ch = tokenizer.lookUpChar(offset);
		return Character.isJavaIdentifierStart(ch) && ch != '\0' && ch != '$';
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		int i = offset;
		char ch = '\0';
		while(Character.isJavaIdentifierPart(ch = tokenizer.readNextChar()) && ch != '\0') {
			i++;
		}
		if(ch != '\0') {
			tokenizer.releaseChar();
		}
		tokenizer.addToken(getType(), offset, i);
		return false;
	}

}

class InstanceofTokenDescription extends ConstantTokenDescription {
	public static final int INSTANCEOF = 16;

	public static InstanceofTokenDescription INSTANCE = new InstanceofTokenDescription();

	private static final String[] OPS_2 = {
		"instanceof" //$NON-NLS-1$
	};

	public InstanceofTokenDescription() {
		super("!", INSTANCEOF); //$NON-NLS-1$
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
