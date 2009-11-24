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
public class WhiteSpaceTokenDescription implements ITokenDescription {
	public static final int WHITESPACE = 0;

	public static WhiteSpaceTokenDescription INSTANCE = new WhiteSpaceTokenDescription();

	public String getName() {
		return ElCoreMessages.WhiteSpaceTokenDescription_Name;
	}

	public int getType() {
		return WHITESPACE;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		char i = tokenizer.lookUpChar(offset);
		return Character.isWhitespace(i);
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		int i = offset;
		char ch = '\0';
		while(Character.isWhitespace(ch = tokenizer.readNextChar())) {
			i++;
		}
		if(ch != '\0') {
			tokenizer.releaseChar();
		}
		tokenizer.addToken(getType(), offset, i);
		return false;
	}
	
}
