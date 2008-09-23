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
package org.jboss.tools.common.el.core.model;

import org.jboss.tools.common.el.core.parser.LexicalToken;

/**
 * Argument object includes opening token, the argument expression proper, 
 * and closing token:
 * '[' expression ']'
 * 
 * @author V. Kabanovich
 *
 */
public interface ELArgument {

	public LexicalToken getOpenArgumentToken();

	public ELExpression getArgument();

	public LexicalToken getCloseArgumentToken();

}
