/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.validation.java;

import java.util.Map;

import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitDocumentProvider.ProblemAnnotation;

/**
 * @author Victor V. Rubezhny
 */
public class TempJavaProblemAnnotation extends ProblemAnnotation {
	private TempJavaProblem problem;

	public TempJavaProblemAnnotation(TempJavaProblem problem, ICompilationUnit cu) {
		super(problem, cu);
		this.problem = problem;
	}

	@SuppressWarnings("rawtypes")
	public Map getAttributes() {
		return problem.getAttributes();
	}
}