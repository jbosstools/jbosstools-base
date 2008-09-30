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
public class JavaNameTokenDescription implements ITokenDescription {
	public static final int JAVA_NAME = 3;

	public static JavaNameTokenDescription INSTANCE = new JavaNameTokenDescription();

	public String getName() {
		return "NAME";
	}

	public int getType() {
		return JAVA_NAME;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		char ch = tokenizer.lookUpChar(offset);
		return Character.isJavaIdentifierStart(ch) && ch != '\0';
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
