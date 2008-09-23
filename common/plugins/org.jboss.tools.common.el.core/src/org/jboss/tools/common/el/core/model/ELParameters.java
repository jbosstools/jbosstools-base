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

/**
 * Details of parameters in method invocation 
 * '(' (expr (, expr)*)? ')'
 * 
 * @author V. Kabanovich
 *
 */
public interface ELParameters extends ELObject {

	public LexicalToken getOpenParametersToken();

	public List<ELExpression> getParameters();

	public LexicalToken getCloseParametersToken();

}
