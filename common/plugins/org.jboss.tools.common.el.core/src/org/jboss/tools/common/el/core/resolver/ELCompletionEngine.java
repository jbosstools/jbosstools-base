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
package org.jboss.tools.common.el.core.resolver;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.BadLocationException;
import org.jboss.tools.common.el.core.model.ELExpression;

public interface ELCompletionEngine extends ELResolver {

	public ELResolution resolveELOperand(IFile file, ELExpression operand,  
			boolean returnEqualedVariablesOnly, List<Var> vars, ElVarSearcher varSearcher) throws BadLocationException, StringIndexOutOfBoundsException;
}