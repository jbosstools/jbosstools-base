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
public class StartELTokenDescription extends ConstantTokenDescription {
	public static final int START_EL = 1;

	public static StartELTokenDescription INSTANCE = new StartELTokenDescription();

	public StartELTokenDescription () {
		super("#{", START_EL); //$NON-NLS-1$
		addContent("${"); //$NON-NLS-1$
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		boolean b = super.read(tokenizer, offset);
		if(b) {
			ParamUtil.createParamHistory(tokenizer.getContext());
		}
		return b;
	}

}
