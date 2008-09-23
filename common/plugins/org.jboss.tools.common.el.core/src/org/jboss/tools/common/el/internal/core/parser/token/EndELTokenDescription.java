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
import org.jboss.tools.common.el.internal.core.parser.rule.BasicStates;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class EndELTokenDescription extends ConstantTokenDescription {
	public static final int END_EL = 2;

	public static EndELTokenDescription INSTANCE = new EndELTokenDescription();

	public EndELTokenDescription () {
		super("}", END_EL);
	}

	public boolean isStart(Tokenizer tokenizer, int offset) {
		if(!super.isStart(tokenizer, offset)) {
			return false;
		}
		if(tokenizer.getState() == BasicStates.STATE_ERROR) {
			return true;
		}
		return ParamUtil.isParamHistoryEmpty(tokenizer.getContext());
	}

	public boolean read(Tokenizer tokenizer, int offset) {
		boolean b = super.read(tokenizer, offset);
		if(b) {
			ParamUtil.cleanParamHistory(tokenizer.getContext());
		}
		return b;
	}

}
