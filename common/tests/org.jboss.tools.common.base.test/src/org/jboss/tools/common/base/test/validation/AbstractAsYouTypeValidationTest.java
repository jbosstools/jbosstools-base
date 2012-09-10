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
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
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
import org.eclipse.wst.validation.internal.operations.EnabledValidatorsOperation;
import org.jboss.tools.common.validation.ValidatorManager;
import org.jboss.tools.test.util.JobUtils;
import org.jboss.tools.test.util.WorkbenchUtils;

/**
 * 
 * @author Victor V. Rubezhny
 *
 */
@SuppressWarnings("restriction")
public abstract class AbstractAsYouTypeValidationTest extends TestCase {
	public static final int MAX_SECONDS_TO_WAIT = 5;

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
		IFile testfile = project.getFile(fileName);
//		System.out.println(testfile.toString() + ": testfile.exists(): " + testfile.exists() + ", testfile.isAccessible(): " + testfile.isAccessible());
		assertTrue("Test file doesn't exist: " + project.getName() + "/" + fileName, 
				(testfile.exists() && testfile.isAccessible()));
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

	public IDocument getDocument() {
		return document;
	}
	public IFile getFile() {
		return file;
	}
	
	protected abstract ISourceViewer getTextViewer();

	/**
	 * The test procedure steps:
	 * - Find EL by a given number
	 * - Set up a good EL => see no annotations on that EL
	 * - Set up a broken EL => see annotation appearance on that EL
	 * - Set up a good EL again => see annotation to disappear on that EL
	 * 
	 * @param goodEL
	 * @param elToValidate
	 * @param errorMessage
	 * @param numberOfRegionToTest
	 * @return boolean indicating that test was done for a region with a given number
	 * @throws JavaModelException
	 * @throws BadLocationException
	 */
	public boolean doAsYouTypeValidationTest(String goodEL, String elToValidate,
			String errorMessage, int numberOfRegionToTest) throws JavaModelException, BadLocationException {

		//============================
		// The test procedure steps:
		// - Find EL by a given number
		//============================
		
		String documentContent = document.get();
		int count = -1; 
		
		int start = 0;
		int end = 0;
		while (++count <= numberOfRegionToTest) {
			start = (documentContent == null ? -1 : documentContent
					.indexOf(EL2FIND_START, end));
			if (start == -1 && count == numberOfRegionToTest)
				return false;
			assertFalse("No EL found in Java Strings: Starting '" + EL2FIND_START
					+ "' characters are not found in document", (start == -1));
			end = (documentContent == null ? -1 : documentContent.indexOf(
					EL2FIND_END, start));
			if (end == -1 && count == numberOfRegionToTest)
				return false;
			assertFalse("EL is not closed in Java Strings: Ending '"
					+ EL2FIND_START + "' characters are not found in document",
					(end == -1));
		}
		if (count < numberOfRegionToTest) {
			// No more regions to test
			return false;
		}
		
		int length = end - start + EL2FIND_END.length();

		//==================================================
		// - Set up a good EL => see no annotations on that EL
		//==================================================

//		System.out.println("Text to be replaced [0]: [" + document.get(start, length) + "]");
		document.replace(start, length, goodEL);
		Annotation problemAnnotation = waitForAnnotation(
				start, end, null, MAX_SECONDS_TO_WAIT, false, false);
		assertNull("Problem Annotation found on a good EL!", problemAnnotation);
		length = goodEL.length();

		//==============================================================
		// - Set up a broken EL => see annotation appearance on that EL
		//==============================================================

//		System.out.println("Text to be replaced [1]: [" + document.get(start, length) + "]");
		document.replace(start, length, elToValidate);
		length = elToValidate.length();
		end = start + elToValidate.length();

		problemAnnotation = waitForAnnotation(
				start, end, errorMessage, MAX_SECONDS_TO_WAIT, false, true);
		assertNotNull("No Problem Annotation found!", problemAnnotation);

		String message = problemAnnotation.getText();
		assertEquals(
				"Not expected error message found in ProblemAnnotation. Expected: ["
						+ errorMessage + "], Found: [" + message + "]",
				errorMessage, message);
		
		//===================================================================
		// - Set up a good EL again => see annotation to disappear on that EL
		//===================================================================

//		System.out.println("Text to be replaced [2]: [" + document.get(start, length) + "]");
		document.replace(start, length, goodEL);
		problemAnnotation = waitForAnnotation(
				start, end, null, MAX_SECONDS_TO_WAIT, false, false);
		assertNull("Problem Annotation has not disappeared!", problemAnnotation);

		return true;
	}

	/**
	 * The test procedure steps:
	 * - Find EL by a given number
	 * - Set up a broken EL and save the document => see problem marker appearance on that EL
	 * - Set up a another broken EL => see annotation appearance on that EL instead of a problem marker 
	 *   (an old problem marker has to disappear)
	 * - Set up a good EL again => see annotation to disappear on that EL
	 * 
	 * @param goodEL
	 * @param elToValidate
	 * @param errorMessage
	 * @param numberOfRegionToTest
	 * @throws BadLocationException
	 * @throws CoreException 
	 */
	public boolean doAsYouTypeValidationMarkerAnnotationsRemovalTest(String goodEL, String elToValidate,
			String errorMessage, String anotherELToValidate, String anotherErrorMessage, int numberOfRegionToTest) throws BadLocationException, CoreException {

//		System.out.println("doAsYouTypeValidationMarkerAnnotationsRemovalTest(goodEL=" + goodEL + ", elToValidate=" + elToValidate + ", errorMessage=[" + errorMessage + 
//				"],\n\tanotherELToValidate=" + anotherELToValidate+ ", anotherErrorMessage=[" + anotherErrorMessage + "], numberOfRegionToTest=" + numberOfRegionToTest + ")");
		//============================
		// The test procedure steps:
		// - Find EL by a given number
		//============================
		
		String documentContent = document.get();
		int count = -1; 
		
		int start = 0;
		int end = 0;
		while (++count <= numberOfRegionToTest) {
			start = (documentContent == null ? -1 : documentContent
					.indexOf(EL2FIND_START, end));
			if (start == -1 && count == numberOfRegionToTest)
				return false;
			assertFalse("No EL found in Java Strings: Starting '" + EL2FIND_START
					+ "' characters are not found in document", (start == -1));
			end = (documentContent == null ? -1 : documentContent.indexOf(
					EL2FIND_END, start));
			if (end == -1 && count == numberOfRegionToTest)
				return false;
			assertFalse("EL is not closed in Java Strings: Ending '"
					+ EL2FIND_START + "' characters are not found in document",
					(end == -1));
		}
		if (count < numberOfRegionToTest) {
			// No more regions to test
			return false;
		}
			
		int length = end - start + EL2FIND_END.length();

		//========================================================================================
		//  - Set up a broken EL and save the document => see problem marker appearance on that EL
		//========================================================================================

//		System.out.println("Text to be replaced [0]: [" + document.get(start, length) + "]");
//		document.replace(start, length, elToValidate);
		
//		document = viewer.getDocument(); // Probably we should find that EL again because the document may be re-formatted at save (?)
//		end = start + elToValidate.length();
//		length = elToValidate.length();

		// do check marker and marker annotation appeared here
		int line = document.getLineOfOffset(start);
		assertResourceMarkerIsCreated(file, errorMessage, line + 1);

		Annotation problemAnnotation = waitForAnnotation(
				start, end, errorMessage, MAX_SECONDS_TO_WAIT, true, true);
		assertNotNull("Problem Marker Annotation not found!", problemAnnotation);
		
		String message = problemAnnotation.getText();
		assertEquals(
				"Not expected error message found in ProblemAnnotation. Expected: ["
						+ errorMessage + "], Found: [" + message + "]",
				errorMessage, message);
		
		//=================================================================================================
		// - Set up a another broken EL => see annotation appearance on that EL instead of a problem marker 
		//   (an old problem marker has to disappear)
		//=================================================================================================

//		System.out.println("Text to be replaced [1]: [" + document.get(start, length) + "] by [" + anotherELToValidate + "]");
		document.replace(start, length, anotherELToValidate);
		
		end = start + anotherELToValidate.length();
		length = anotherELToValidate.length();

		problemAnnotation = waitForAnnotation(
				start, end, anotherErrorMessage, MAX_SECONDS_TO_WAIT, false, true);
		
		assertNotNull("No Problem Annotation found for EL " + anotherELToValidate + " on region " + numberOfRegionToTest + "!", problemAnnotation);

		message = problemAnnotation.getText();
		assertEquals(
				"Not expected error message found in ProblemAnnotation. Expected: ["
						+ anotherErrorMessage + "], Found: [" + message + "]",
				anotherErrorMessage, message);

		// do check marker annotation has disappeared here
		problemAnnotation = waitForAnnotation(
				start, end, null, MAX_SECONDS_TO_WAIT, true, false);
		assertNull("Problem Marker Annotation has not disappeared!", problemAnnotation);

		//===================================================================
		// - Set up a good EL again => see annotation to disappear on that EL
		//===================================================================

//		System.out.println("Text to be replaced [2]: [" + document.get(start, length) + "] by [" + goodEL + "]");
		document.replace(start, length, goodEL);
		problemAnnotation = waitForAnnotation(
				start, end, null, MAX_SECONDS_TO_WAIT, false, false);
		assertNull("Problem Annotation has not disappeared!", problemAnnotation);

		return true;
	}
	
	public Annotation waitForAnnotation(final int start, final int end, final String errorMessage, final int seconds, final boolean markerAnnotation, final boolean waitForAppearance) {
		final Annotation[] result = new Annotation[] { null };

		Display.getDefault().syncExec(new Runnable() {
			@SuppressWarnings("rawtypes")
			public void run() {
				int secondsLeft = seconds;
				boolean isFirstPass = true;
				while (secondsLeft-- > 0) {
					if (!isFirstPass || waitForAppearance) {
						JobUtils.delay(1000);

						// clean deffered events
						while (Display.getCurrent().readAndDispatch())
							;
					} else {
						secondsLeft++; // because the wait step was skipped
					}

					annotationModel = getAnnotationModel();
					if (annotationModel == null) 
						return;
					
					Iterator it = annotationModel.getAnnotationIterator();
					boolean found = false;
					while (it.hasNext()) {
						Object o = it.next();

						if (!(o instanceof Annotation))
							continue;
						
						Annotation annotation = (Annotation) o;
						Position position = annotationModel
								.getPosition(annotation);
						if (position == null)
							continue;

						if (position.getOffset() < start
								|| position.getOffset() >= end)
							continue;

						if (position.getOffset() + position.getLength() > end)
							continue;

						if (!markerAnnotation && errorMessage != null && !errorMessage.equals(annotation.getText()))
							continue;

//						System.out.println("A: " + (position == null ? null : position.toString()) + ", " + annotation.getClass().getName() + ", " + annotation.getType() + ", " + annotation.getText());

						found = markerAnnotation ? isMarkerAnnotationAcceptable(annotation) : isAnnotationAcceptable(annotation);
						if (found) {
							if (waitForAppearance) {
								// Return the annotation we've searched for
								result[0] = (Annotation)o;
								return;
							} else {
								// Continue to wait for annotation to disappear
								break;
							}
						}
					}
					
					// if waiting for an annotation to disappear then don't return at first pass
					if (!found && !waitForAppearance && !isFirstPass) {
						return; // Annotation not found or disappeared
					}					
					isFirstPass = false;
				}
			}
		});

//		System.out.println(result[0] == null ? "Not found":"found");
		return result[0];
	}
	
	protected void waitForValidation(IProject project) throws CoreException{
		ValidatorManager.setStatus(ValidatorManager.RUNNING);
		IProgressMonitor monitor = new NullProgressMonitor();
		project.refreshLocal(IResource.DEPTH_INFINITE, monitor);
		project.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, monitor);
		new EnabledValidatorsOperation(project,false).run(monitor);
		JobUtils.waitForIdle();
	}
	
	protected String modifyModifyELInContent(StringBuilder content, String newEL) {
		if (content == null)
			return null;
		
		int start = 0;
		int end = 0;
		while (start != -1) {
			start = content.indexOf(EL2FIND_START, end);
			if (start == -1)
				break;

			end = content.indexOf(EL2FIND_END, start);
			if (end == -1)
				break;
			
			content.replace(start, end+1, newEL);
		}
		return content.toString();
	}


	abstract protected boolean isAnnotationAcceptable(Annotation annotation);

	abstract protected boolean isMarkerAnnotationAcceptable(Annotation annotation);
	
	abstract protected void assertResourceMarkerIsCreated(IFile file, String errorMessage, int line) throws CoreException;
}