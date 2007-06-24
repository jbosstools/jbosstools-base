/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.templates;

import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.internal.corext.template.java.CompilationUnitContext;
import org.eclipse.jdt.internal.corext.template.java.JavaContextType;
import org.eclipse.jface.text.IDocument;


/**
 * @author au
 */

public class RedHatTemplateContextType extends JavaContextType {

	public CompilationUnitContext createContext(IDocument document, int offset,	int length, ICompilationUnit compilationUnit) {
		return super.createContext(document, offset, length, compilationUnit);
	}
}
