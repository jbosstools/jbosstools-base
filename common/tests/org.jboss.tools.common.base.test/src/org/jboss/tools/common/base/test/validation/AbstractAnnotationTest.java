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
package org.jboss.tools.common.base.test.validation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import junit.framework.TestCase;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.sse.ui.StructuredTextEditor;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.base.test.BaseTestPlugin;
import org.jboss.tools.common.util.IEditorWrapper;
import org.jboss.tools.common.validation.AsYouTypeValidatorManager;
import org.jboss.tools.common.validation.TempMarkerManager;
import org.jboss.tools.test.util.WorkbenchUtils;
import org.jboss.tools.tests.AbstractResourceMarkerTest;
import org.jboss.tools.tests.IAnnotationTest;

/**
 * @author Alexey Kazakov
 */
public abstract class AbstractAnnotationTest extends TestCase implements IAnnotationTest {

	private Set<IResource> validatedResources = new HashSet<IResource>();

	protected abstract String getMarkerType();

	private ISourceViewer getTextViewer(IEditorPart editorPart) {
		ISourceViewer viewer = null;
		ITextEditor textEditor = null;
		if (editorPart instanceof IEditorWrapper) {
			editorPart = ((IEditorWrapper) editorPart).getEditor();
		}
		if (editorPart instanceof ITextEditor) {
			textEditor = (ITextEditor) editorPart;
		} else {
			textEditor = editorPart == null ? null : (ITextEditor)editorPart.getAdapter(ITextEditor.class);
		}
		if(textEditor instanceof JavaEditor) {
			viewer = ((JavaEditor)textEditor).getViewer();
		} else if(textEditor instanceof StructuredTextEditor) {
			viewer = ((StructuredTextEditor)textEditor).getTextViewer();
		}
		return viewer;
	}

	private void modifyDocument(IDocument document) throws BadLocationException {
		String s = document.get(document.getLength()-1, 1);
		document.replace(document.getLength() - 1, 1, s + " ");
	}

	public void assertAnnotationsEqualToMarkers(final IResource resource) throws CoreException {
		if(validatedResources.contains(resource)) {
			return;
		}
		validatedResources.add(resource);

		IMarker[] allMarkers = AbstractResourceMarkerTest.findMarkers(resource, getMarkerType(), ".*", true);
		StringBuffer markersSB = new StringBuffer("Marker annotations: [\r\n"); //$NON-NLS-1$
		int i=0;
		for (IMarker marker : allMarkers) {
			String message = marker.getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
			int line = marker.getAttribute(IMarker.LINE_NUMBER, -1);
			String mType = marker.getType();
			int startInt = marker.getAttribute(IMarker.CHAR_START, -1);
			int endInt = marker.getAttribute(IMarker.CHAR_END, -1);
			markersSB.append(++i).append(") message=\"").append(message).append("\"; line=\"").append(line).append("\"; start=\"").append(startInt).append("\"; end=\"").append(endInt).append("\"; type=\"").append(mType).append("\";\r\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		}
		markersSB.append("]"); //$NON-NLS-1$

		IEditorPart editorPart = WorkbenchUtils.openEditor(resource.getFullPath());
		assertNotNull(editorPart);
		AsYouTypeValidatorManager manager = new AsYouTypeValidatorManager() {
			public void validateString(Collection<IRegion> dirtyRegions, IValidationContext helper, IReporter reporter) {
				validateString(dirtyRegions, helper, reporter, true);
			}
			public void validateJavaElement(Collection<IRegion> dirtyRegions, IValidationContext helper, IReporter reporter) {
				validateJavaElement(dirtyRegions, helper, reporter, true);
			}
		};
		IDocument document = null;

		try {
			AsYouTypeValidatorManager.setDisabled(true);
			ISourceViewer viewer = getTextViewer(editorPart);
			assertNotNull(viewer);
			document = viewer.getDocument();
			manager.connect(document);
			modifyDocument(document);

			Collection<IRegion> dirtyRegions = new ArrayList<IRegion>();
			final IDocument finalDocument = document;
			dirtyRegions.add(new IRegion() {
				@Override
				public int getOffset() {
					return 0;
				}
				@Override
				public int getLength() {
					return finalDocument.getLength();
				}
			});

			IValidationContext helper = new IValidationContext() {
				@Override
				public Object loadModel(String symbolicName) {
					return null;
				}
				@Override
				public Object loadModel(String symbolicName, Object[] parms) {
					return null;
				}
				@Override
				public String[] getURIs() {
					return new String[] {resource.getFullPath().toString()};
				}
			};

			IReporter reporter = new IReporter() {
				private List<IMessage> messages = new ArrayList<IMessage>();
				@Override
				public void addMessage(IValidator origin, IMessage message) {
					messages.add(message);
				}
				@Override
				public void displaySubtask(IValidator validator, IMessage message) {
				}
				@Override
				public List getMessages() {
					return messages;
				}
				@Override
				public boolean isCancelled() {
					return false;
				}
				@Override
				public void removeAllMessages(IValidator origin) {
				}
				@Override
				public void removeAllMessages(IValidator origin, Object object) {
				}
				@Override
				public void removeMessageSubset(IValidator validator, Object obj, String groupName) {
				}
			};

			manager.validateJavaElement(dirtyRegions, helper, reporter);
//			manager.validateString(dirtyRegions, helper, reporter);

			StringBuffer messagesSB = new StringBuffer("AYT annotations: [\r\n"); //$NON-NLS-1$
			List<IMessage> messages = new ArrayList<IMessage>();
			i=0;
			for (Object object : reporter.getMessages()) {
				IMessage message = (IMessage)object;
				Object mType = message.getAttribute(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME);
				if(getMarkerType().equals(mType)) {
					messages.add(message);
					String text = message.getText();
					int line = message.getLineNumber();
					int start = message.getOffset();
					int end = start + message.getLength();
					messagesSB.append(++i).append(") message=\"").append(text).append("\"; line=\"").append(line).append("\"; start=\"").append(start).append("\"; end=\"").append(end).append("\"; type=\"").append(mType).append("\";\r\n"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
				}
			}
			messagesSB.append("]"); //$NON-NLS-1$

			assertEquals("The number of markers doesn't equal to the number of AYT messages for " + resource + ".\r\nThe full list of the markers and messages:\r\n" + markersSB + "\r\n" + messagesSB, allMarkers.length, messages.size());

			for (IMessage message : messages) {
				assertTrue("Can't find message: [" + message + "] for " + resource + ".\r\nThe full list of the markers and messages:\r\n" + markersSB + "\r\n" + messagesSB, contains(allMarkers, message));
			}
		} catch (BadLocationException e) {
			throw new CoreException(new Status(IStatus.ERROR, BaseTestPlugin.PLUGIN_ID, e.getMessage(), e));
		} finally {
			AsYouTypeValidatorManager.setDisabled(false);
			if(document!=null) {
				manager.disconnect(document);
			}
			if (editorPart != null) {
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeEditor(editorPart, false);
			}
		}
	}

	private boolean contains(IMarker[] markers, IMessage message) throws CoreException {
		for (IMarker marker : markers) {
			if(equaled(marker, message)) {
				return true;
			}
		}
		return false;
	}

	private boolean equaled(IMarker marker, IMessage message) throws CoreException {
		String markerText = marker.getAttribute(IMarker.MESSAGE, ""); //$NON-NLS-1$
		int markerLine = marker.getAttribute(IMarker.LINE_NUMBER, -1);
		int markerStart = marker.getAttribute(IMarker.CHAR_START, -1);
		int markerEnd = marker.getAttribute(IMarker.CHAR_END, -1);

		String messageText = message.getText();
		int messageLine = message.getLineNumber();
		int messageStart = message.getOffset();
		int messageEnd = messageStart + message.getLength();

		return markerText.equals(messageText) && markerLine==messageLine && markerStart==messageStart && markerEnd==messageEnd;
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.tests.IAnnotationTest#assertAnnotationIsCreated(org.eclipse.core.resources.IResource, java.lang.String, int[])
	 */
	@Override
	public void assertAnnotationIsCreated(IResource resource, String pattern, int... expectedLines) throws CoreException {
		assertAnnotationsEqualToMarkers(resource);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.tests.IAnnotationTest#assertAnnotationIsCreated(org.eclipse.core.resources.IResource, java.lang.String, boolean, int[])
	 */
	@Override
	public void assertAnnotationIsCreated(IResource resource, String message, boolean pattern, int... expectedLines) throws CoreException {
		assertAnnotationsEqualToMarkers(resource);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.tests.IAnnotationTest#assertAnnotationIsNotCreated(org.eclipse.core.resources.IResource, java.lang.String)
	 */
	@Override
	public void assertAnnotationIsNotCreated(IResource resource, String message) throws CoreException {
		assertAnnotationsEqualToMarkers(resource);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.tests.IAnnotationTest#assertAnnotationIsNotCreated(org.eclipse.core.resources.IResource, java.lang.String, int)
	 */
	@Override
	public void assertAnnotationIsNotCreated(IResource resource, String message, int expectedLine) throws CoreException {
		assertAnnotationsEqualToMarkers(resource);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.tests.IAnnotationTest#assertAnnotationIsCreatedForGivenPosition(org.eclipse.core.resources.IResource, java.lang.String, int, int, int)
	 */
	@Override
	public void assertAnnotationIsCreatedForGivenPosition(IResource resource, String message, int lineNumber, int startPosition, int endPosition) throws CoreException {
		assertAnnotationsEqualToMarkers(resource);
	}
}