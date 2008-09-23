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
public class ArgEndTokenDescription extends ConstantTokenDescription {
	public static final int ARG_END = 11;
	public static final String CLOSE = "]";

	public static ArgEndTokenDescription INSTANCE = new ArgEndTokenDescription();

	public ArgEndTokenDescription () {
		super(CLOSE, ARG_END);
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		return isInArg(tokenizer) && super.isStart(tokenizer, offset);
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		boolean b = super.read(tokenizer, offset);
		if(b) {
			tokenizer.getContext().remove("arg");
		}
		return b;
	}

	private boolean isInArg(Tokenizer tokenizer) {
		return tokenizer.getContext().get("arg") != null;
	}

}
