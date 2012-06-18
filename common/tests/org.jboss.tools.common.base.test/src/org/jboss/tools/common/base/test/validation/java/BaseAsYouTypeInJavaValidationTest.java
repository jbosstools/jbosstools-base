/*******************************************************************************
 * Copyright (c) 2012 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.base.test.validation.java;

import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.internal.ui.javaeditor.ClassFileEditor;
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitDocumentProvider.ProblemAnnotation;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.jboss.tools.common.base.test.validation.AbstractAsYouTypeValidationTest;

/**
 * 
 * @author Victor V. Rubezhny
 *
 */
@SuppressWarnings("restriction")
public class BaseAsYouTypeInJavaValidationTest extends AbstractAsYouTypeValidationTest {
	public static final String MARKER_TYPE = "org.jboss.tools.common.validation.el"; //$NON-NLS-1$

	public BaseAsYouTypeInJavaValidationTest(IProject project) {
		this.project = project;
	}
	public BaseAsYouTypeInJavaValidationTest() {
	}
	
	@Override
	protected void obtainEditor(IEditorPart editorPart) {
		if (!(editorPart instanceof JavaEditor)
				|| editorPart instanceof ClassFileEditor)
			return;

		textEditor = (JavaEditor) editorPart;

		assertNotNull(
				"Cannot get the Java Text Editor instance for Java Class file \"" //$NON-NLS-1$
						+ fileName + "\"", textEditor);

		assertTrue("Java Editor is opened for a binary Java Class",
				textEditor.getEditorInput() instanceof IFileEditorInput);
		file = ((IFileEditorInput) textEditor.getEditorInput()).getFile();
		assertNotNull("Java Editor is opened for a binary Java Class", file);
	}

	protected ISourceViewer getTextViewer() {
		return textEditor instanceof JavaEditor ? ((JavaEditor)textEditor).getViewer() : null;
	}

	@Override
	protected boolean isAnnotationAcceptable(Annotation annotation) {
		if (!(annotation instanceof ProblemAnnotation))
			return false;

		ProblemAnnotation problemAnnotation = (ProblemAnnotation) annotation;

		String markerType = problemAnnotation.getMarkerType();
		if (!MARKER_TYPE.equalsIgnoreCase(markerType))
			return false;

		return true;
	}
}
