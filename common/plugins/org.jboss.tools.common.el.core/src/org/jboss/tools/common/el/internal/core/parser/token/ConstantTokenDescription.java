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

import java.util.ArrayList;
import java.util.List;

import org.jboss.tools.common.el.core.parser.ITokenDescription;
import org.jboss.tools.common.el.core.parser.Tokenizer;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ConstantTokenDescription implements ITokenDescription {
	List<String> content = new ArrayList<String>();
	int type;

	public ConstantTokenDescription(String content, int type) {
		this.content.add(content);
		this.type = type;
	}

	protected void addContent(String content) {
		this.content.add(content);
	}

	public String getName() {
		return content.get(0);
	}

	public int getType() {
		return type;
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		for (String s: content) {
			if(tokenizer.startsWith(s)) {
				return true;
			}
		}
		return false;
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		for (String s: content) {
			if(tokenizer.startsWith(s)) {
				tokenizer.addToken(type, offset, offset + s.length());
				return true;
			}
		}
		return true;
	}

}
