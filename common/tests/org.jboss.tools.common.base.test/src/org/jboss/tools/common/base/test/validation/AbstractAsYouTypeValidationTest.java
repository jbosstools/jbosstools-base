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
package org.jboss.tools.common.base.test.validation;

import java.util.Iterator;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * 
 * @author Victor V. Rubezhny
 *
 */
public abstract class AbstractAsYouTypeValidationTest extends TestCase {
	private static final int MAX_SECONDS_TO_WAIT = 10;

	protected String fileName;
	protected IProject project = null;
	protected IEditorPart editorPart = null;
	protected ITextEditor textEditor = null;
	protected ISourceViewer viewer = null;
	protected IDocument document = null;
	protected IFile file = null;
	IAnnotationModel annotationModel = null;

	public static final String EL2FIND_START = "#{";
	public static final String EL2FIND_END = "}";

	public AbstractAsYouTypeValidationTest(IProject project) {
		this.project = project;
	}
	public AbstractAsYouTypeValidationTest() {
	}
	
	public void openEditor(String fileName) {
		this.fileName = fileName;
		editorPart = WorkbenchUtils.openEditor(project.getName()
				+ "/" + fileName); //$NON-NLS-1$
		obtainEditor(editorPart);

		annotationModel = getAnnotationModel();
		assertNotNull("Cannot find an Annotation Model for the Java Editor",
				annotationModel);

		// clean deffered events
		while (Display.getCurrent().readAndDispatch())
			;

		viewer = getTextViewer();
		document = viewer.getDocument();
	}

	public void closeEditor() {
		if (editorPart != null) {
			PlatformUI.getWorkbench().getActiveWorkbenchWindow()
					.getActivePage().closeEditor(editorPart, false);
			editorPart = null;
			textEditor = null;
			viewer = null;
			document = null;
		}
	}

	protected abstract void obtainEditor(IEditorPart editorPart);
	
	protected IAnnotationModel getAnnotationModel() {
		final IDocumentProvider documentProvider = textEditor.getDocumentProvider();
		if (documentProvider == null) {
			return null;
		}
		return documentProvider.getAnnotationModel(textEditor.getEditorInput());
	}

	protected abstract ISourceViewer getTextViewer();

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

		int length = end - start + EL2FIND_END.length();

		document.replace(start, length, elToValidate);

		end = start + elToValidate.length();

		Annotation problemAnnotation = waitForProblemAnnotationAppearance(
				start, end, MAX_SECONDS_TO_WAIT);
		assertNotNull("No ProblemAnnotation found!", problemAnnotation);

		String message = problemAnnotation.getText();
		assertEquals(
				"Not expected error message found in ProblemAnnotation. Expected: ["
						+ errorMessage + "], Found: [" + message + "]",
				errorMessage, message);
	}

	private Annotation waitForProblemAnnotationAppearance(
			final int start, final int end, final int seconds) {
		final Annotation[] result = new Annotation[] { null };

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
					@SuppressWarnings("rawtypes")
					Iterator it = annotationModel.getAnnotationIterator();
					while (!found && it.hasNext()) {
						Object o = it.next();

						if (!(o instanceof Annotation))
							continue;
						
						Annotation annotation = (Annotation) o;
						Position position = annotationModel
								.getPosition(annotation);

						if (position.getOffset() < start
								|| position.getOffset() >= end)
							continue;

						if (position.getOffset() + position.getLength() >= end)
							continue;

						
						if (!isAnnotationAcceptable(annotation))
							continue;
						
						result[0] = (Annotation)o;
						return;
					}
				}
			}
		});

		return result[0];
	}
	
	abstract protected boolean isAnnotationAcceptable(Annotation annotation);
}
