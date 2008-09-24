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

import java.util.List;

import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.parser.SyntaxError;

/**
 * Instance of EL includes opening token, the expression proper, 
 * and closing token:
 * '#{' expression '}'
 * '${' expression '}'
 * 
 * @author V. Kabanovich
 *
 */
public interface ELInstance extends ELObject {

	public LexicalToken getOpenInstanceToken();

	public ELExpression getExpression();

	public LexicalToken getCloseInstanceToken();

	public List<SyntaxError> getErrors();

}
