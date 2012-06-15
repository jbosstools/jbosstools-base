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

import java.util.Iterator;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.WorkingCopyOwner;
import org.eclipse.jdt.internal.ui.javaeditor.ClassFileEditor;
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitDocumentProvider.ProblemAnnotation;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * 
 * @author Victor V. Rubezhny
 *
 */
public class BaseAsYouTypeInJavaValidationTest extends TestCase {
	private static final int MAX_SECONDS_TO_WAIT = 10;

	protected String fileName;
	protected IProject project = null;
	protected IEditorPart editorPart = null;
	protected JavaEditor javaEditor = null;
	protected ISourceViewer viewer = null;
	protected IDocument document = null;
	protected IFile file = null;
	IAnnotationModel annotationModel = null;

	/** The working copy owner */
	protected final WorkingCopyOwner workingCopyOwner = new WorkingCopyOwner() {
	};

	public static final String MARKER_TYPE = "org.jboss.tools.common.validation.el"; //$NON-NLS-1$
	public static final String EL2FIND_START = "#{";
	public static final String EL2FIND_END = "}";

	public BaseAsYouTypeInJavaValidationTest(IProject project) {
		this.project = project;
	}
	public BaseAsYouTypeInJavaValidationTest() {
	}
	
	public void openEditor(String fileName) {
		this.fileName = fileName;
		editorPart = WorkbenchUtils.openEditor(project.getName()
				+ "/" + fileName); //$NON-NLS-1$
		obtainJavaEditor(editorPart);
		viewer = getTextViewer();
		document = viewer.getDocument();
	}

	public void closeEditor() {
		if (editorPart != null) {
			PlatformUI.getWorkbench().getActiveWorkbenchWindow()
					.getActivePage().closeEditor(editorPart, false);
			editorPart = null;
			javaEditor = null;
			viewer = null;
			document = null;
		}
	}

	protected void obtainJavaEditor(IEditorPart editorPart) {
		if (!(editorPart instanceof JavaEditor)
				|| editorPart instanceof ClassFileEditor)
			return;

		javaEditor = (JavaEditor) editorPart;

		assertNotNull(
				"Cannot get the Java Text Editor instance for Java Class file \"" //$NON-NLS-1$
						+ fileName + "\"", javaEditor);

		assertTrue("Java Editor is opened for a binary Java Class",
				javaEditor.getEditorInput() instanceof IFileEditorInput);
		file = ((IFileEditorInput) javaEditor.getEditorInput()).getFile();
		assertNotNull("Java Editor is opened for a binary Java Class", file);

		annotationModel = getAnnotationModel();
		assertNotNull("Cannot find an Annotation Model for the Java Editor",
				annotationModel);

		// clean deffered events
		while (Display.getCurrent().readAndDispatch())
			;
	}

	protected IAnnotationModel getAnnotationModel() {
		final IDocumentProvider documentProvider = javaEditor
				.getDocumentProvider();
		if (documentProvider == null) {
			return null;
		}
		return documentProvider.getAnnotationModel(javaEditor.getEditorInput());
	}

	protected ISourceViewer getTextViewer() {
		return javaEditor != null ? javaEditor.getViewer() : null;
	}

	public void doAsYouTipeInJavaValidationTest(String elToValidate,
			String errorMessage) throws JavaModelException, BadLocationException {
		String documentContent = document.get();
		int start = (documentContent == null ? -1 : documentContent
				.indexOf(EL2FIND_START));
		assertFalse("No EL found in Java Strings: Starting '" + EL2FIND_START
				+ "' characters are not found in document", (start == -1));
		int end = (documentContent == null ? -1 : documentContent.indexOf(
				EL2FIND_END, start));
		assertFalse("EL is not closed in Java Strings: Ending '"
				+ EL2FIND_START + "' characters are not found in document",
				(end == -1));

		int offset = start;
		int length = end - start + EL2FIND_END.length();

		document.replace(start, length, elToValidate);

		end = start + elToValidate.length();

		ProblemAnnotation problemAnnotation = waitForProblemAnnotationAppearance(
				start, end, MARKER_TYPE, MAX_SECONDS_TO_WAIT);
		assertNotNull("No ProblemAnnotation found for Marker Type: "
				+ MARKER_TYPE, problemAnnotation);

		String message = problemAnnotation.getText();
		assertEquals(
				"Not expected error message found in ProblemAnnotation. Expected: ["
						+ errorMessage + "], Found: [" + message + "]",
				errorMessage, message);
	}

	private ProblemAnnotation waitForProblemAnnotationAppearance(
			final int start, final int end, final String markerType,
			final int seconds) {
		final ProblemAnnotation[] result = new ProblemAnnotation[] { null };

		Display.getDefault().syncExec(new Runnable() {
			public void run() {
				int secondsLeft = seconds;
				while (secondsLeft-- > 0) {
					JobUtils.delay(1000);

					// clean deffered events
					while (Display.getCurrent().readAndDispatch())
						;

					annotationModel = getAnnotationModel();
					boolean found = false;
					Iterator it = annotationModel.getAnnotationIterator();
					while (!found && it.hasNext()) {
						Object o = it.next();

						if (!(o instanceof ProblemAnnotation))
							continue;

						ProblemAnnotation problemAnnotation = (ProblemAnnotation) o;
						Position position = annotationModel
								.getPosition(problemAnnotation);

						if (position.getOffset() < start
								|| position.getOffset() >= end)
							continue;

						if (position.getOffset() + position.getLength() >= end)
							continue;

						String paMarkerType = problemAnnotation.getMarkerType();
						if (!markerType.equalsIgnoreCase(markerType))
							continue;

						System.out.println("Problem: " + problemAnnotation.getText());
						result[0] = problemAnnotation;
						return;
					}
				}
			}
		});

		return result[0];
	}

}
