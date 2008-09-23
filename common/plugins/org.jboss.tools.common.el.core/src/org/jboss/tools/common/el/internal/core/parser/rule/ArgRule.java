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
package org.jboss.tools.common.el.internal.core.parser.rule;

import org.jboss.tools.common.el.core.parser.IRule;
import org.jboss.tools.common.el.internal.core.parser.token.ArgEndTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.PrimitiveValueTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.StringTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.WhiteSpaceTokenDescription;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ArgRule implements IRule, BasicStates {
	
	public static ArgRule INSTANCE = new ArgRule();

	public int[] getStartStates() {
		return new int[] {
			STATE_EXPECTING_ARG,
			STATE_EXPECTING_ARG_CLOSE,
		};
	}

	public int getFinalState(int state, int token) {
		switch (token) {
			case WhiteSpaceTokenDescription.WHITESPACE: 
					return state;
			case PrimitiveValueTokenDescription.PRIMITIVE_VALUE:
			case StringTokenDescription.STRING:
					return STATE_EXPECTING_ARG_CLOSE;
			case ArgEndTokenDescription.ARG_END:
					return STATE_EXPECTING_CALL_AFTER_METHOD;
		}

		return 0;
	}

	public int[] getTokenTypes(int state) {
		switch(state) {
			case STATE_EXPECTING_ARG:
				return new int[] {
					WhiteSpaceTokenDescription.WHITESPACE,
					PrimitiveValueTokenDescription.PRIMITIVE_VALUE,					
					StringTokenDescription.STRING,
				};
			case STATE_EXPECTING_ARG_CLOSE:
				return new int[] {
					WhiteSpaceTokenDescription.WHITESPACE,
					ArgEndTokenDescription.ARG_END,
				};
		}
		return new int[0];
	}

}
