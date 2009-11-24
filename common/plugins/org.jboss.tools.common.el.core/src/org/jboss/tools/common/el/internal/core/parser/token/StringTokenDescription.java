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
public class StringTokenDescription implements ITokenDescription {
	public static final int STRING = 5;

	public static StringTokenDescription INSTANCE = new StringTokenDescription();

	public String getName() {
		return ElCoreMessages.StringTokenDescription_Name;
	}

	public int getType() {
		return STRING;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		char ch = tokenizer.lookUpChar(offset);
		return ch == '"' || ch == '\'';
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		char ch = tokenizer.readNextChar();
		char chE = ch;
		int i = offset;
		while(ch != '\0') {
			i++;
			ch = tokenizer.readNextChar();
			if(ch == '\\') {
				i++;
				ch = tokenizer.readNextChar();
			} else if(ch == chE) {
				i++;
				break;
			}			
		}
		tokenizer.addToken(getType(), offset, i);
		return i > offset;
	}

}
