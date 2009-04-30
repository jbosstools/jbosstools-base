/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import java.util.List;

import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.text.TextProposal;

/**
 * Represents EL Resolver
 * @author Alexey Kazakov
 */
public interface ELResolver {

	/**
	 * 
	 * @param elString
	 * @param position
	 * @param context
	 * @return
	 */
	List<TextProposal> getCompletions(String elString, int position, ELContext context);

	/**
	 * 
	 * @param operand
	 * @param context
	 * @return
	 */
	ELOperandResolveStatus resolveELOperand(ELExpression operand, ELContext context);
}