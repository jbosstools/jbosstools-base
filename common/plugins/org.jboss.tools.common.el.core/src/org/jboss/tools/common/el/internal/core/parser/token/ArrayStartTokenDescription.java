/******************************************************************************* 
 * Copyright (c) 2014 Red Hat, Inc. 
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
public class ArrayStartTokenDescription extends ConstantTokenDescription {
	public static final int ARRAY_START = 16;
	public static final String OPEN = "["; //$NON-NLS-1$
	
	public static ArrayStartTokenDescription INSTANCE = new ArrayStartTokenDescription();

	public ArrayStartTokenDescription () {
		super(OPEN, ARRAY_START);
	}

	@Override
	public boolean read(Tokenizer tokenizer, int offset) {
		boolean b = super.read(tokenizer, offset);
		if(b) {
			ParamUtil.openArrayContext(tokenizer.getContext());
		}
		return b;
	}

}
