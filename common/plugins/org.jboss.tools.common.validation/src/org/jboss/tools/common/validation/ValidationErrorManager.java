/*******************************************************************************
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.validation;

import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.wst.validation.internal.operations.WorkbenchReporter;
import org.eclipse.wst.validation.internal.plugin.ValidationPlugin;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.preferences.SeverityPreferences;
import org.jboss.tools.common.text.ITextSourceReference;

/**
 * @author Alexey Kazakov
 */
public abstract class ValidationErrorManager implements IValidationErrorManager {

	public static String DEFAULT_VALIDATION_MARKER = ValidationPlugin.PLUGIN_ID + ".problemmarker"; //$NON-NLS-1$
	static String VALIDATION_MARKER_OWNER = "owner"; //$NON-NLS-1$
	static String VALIDATION_MARKER_GROUP = "groupName"; //$NON-NLS-1$

	protected IStatus OK_STATUS = new Status(IStatus.OK,
			"org.eclipse.wst.validation", 0, "OK", null); //$NON-NLS-1$ //$NON-NLS-2$

	protected IValidator validationManager;
	protected ContextValidationHelper coreHelper;
	protected IReporter reporter;
	protected IProject validatingProject;
	protected String markerId;
	protected IProjectValidationContext validationContext;
	protected TextFileDocumentProvider documentProvider;

	private String messageIdQuickFixAttributeName;

	/**
	 * Constructor
	 */
	public ValidationErrorManager() {
	}

	/**
	 * @param messageIdQuickFixAttributeName the messageIdQuickFixAttributeName to set
	 */
	public void setMessageIdQuickFixAttributeName(
			String messageIdQuickFixAttributeName) {
		this.messageIdQuickFixAttributeName = messageIdQuickFixAttributeName;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationErrorManager#init(org.eclipse.core.resources.IProject, org.jboss.tools.jst.web.kb.internal.validation.ContextValidationHelper, org.jboss.tools.jst.web.kb.validation.IProjectValidationContext, org.eclipse.wst.validation.internal.provisional.core.IValidator, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void init(IProject project, ContextValidationHelper validationHelper, IProjectValidationContext validationContext, IValidator manager, IReporter reporter) {
		cleanSavedMarkers();
		setProject(project);
		setCoreHelper(validationHelper);
		setValidationManager(manager);
		setReporter(reporter);
		setValidationContext(validationContext);
		setMarkerId(org.jboss.tools.common.validation.IValidator.MARKED_RESOURCE_MESSAGE_GROUP);
	}

	public void init(IProject project, ContextValidationHelper validationHelper, IProjectValidationContext validationContext, IValidator manager, IReporter reporter, String messageIdQuickFixAttributeName) {
		this.init(project, validationHelper, validationContext, manager, reporter);
		setMessageIdQuickFixAttributeName(messageIdQuickFixAttributeName);
	}

	/**
	 * @param validationManager the validationManager to set
	 */
	public void setValidationManager(IValidator validationManager) {
		this.validationManager = validationManager;
	}

	/**
	 * @param coreHelper the coreHelper to set
	 */
	public void setCoreHelper(ContextValidationHelper coreHelper) {
		this.coreHelper = coreHelper;
	}

	/**
	 * @param reporter the reporter to set
	 */
	public void setReporter(IReporter reporter) {
		this.reporter = reporter;
	}

	/**
	 * @param rootProject the rootProject to set
	 */
	public void setProject(IProject rootProject) {
		this.validatingProject = rootProject;
	}

	/**
	 * @param markerId the markerId to set
	 */
	public void setMarkerId(String markerId) {
		this.markerId = markerId;
	}

	/**
	 * @param validationContext the validationContext to set
	 */
	public void setValidationContext(IProjectValidationContext validationContext) {
		this.validationContext = validationContext;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String,
	 *      java.lang.String, java.lang.String[],
	 *      org.jboss.tools.seam.core.ISeamTextSourceReference,
	 *      org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, String preferenceKey,
			String[] messageArguments, ITextSourceReference location,
			IResource target) {
		IResource newTarget = target; 
		if(location.getResource() != null && location.getResource().exists() && !location.getResource().equals(target)) {
				newTarget = location.getResource();
		}
		return addError(message, preferenceKey, messageArguments, 0, location
				.getLength(), location.getStartPosition(), newTarget);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String,
	 *      java.lang.String,
	 *      org.jboss.tools.seam.core.ISeamTextSourceReference,
	 *      org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, String preferenceKey,
			ITextSourceReference location, IResource target) {
		return addError(message, preferenceKey, new String[0], location, target);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String, java.lang.String, java.lang.String[], org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, String preferenceKey,
			String[] messageArguments, IResource target) {
		return addError(message, preferenceKey, messageArguments, 0, 0, 0, target);
	}

	private String getMarkerId() {
		return markerId;
	}

	/**
	 * @param project
	 * @param preferenceKey
	 * @return
	 */
	protected abstract String getPreference(IProject project, String preferenceKey);

	private Set<MarkerID> markers = new HashSet<MarkerID>();

	private static class MarkerID {

		String preferenceKey;
		int length;
		int offset;
		String path;

		public MarkerID(String preferenceKey, int length, int offset, String path) {
			super();
			this.preferenceKey = preferenceKey;
			this.length = length;
			this.offset = offset;
			this.path = path;
		}

		@Override
		public int hashCode() {
			return toString().hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			return obj instanceof MarkerID && toString().equals(obj.toString());
		}

		@Override
		public String toString() {
			return path + ":" + preferenceKey + ":" + length + ":" + offset; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
	}

	/**
	 * Returns true if the manager should not add a problem markers with the same location and preference key twice.
	 * @return
	 */
	protected boolean shouldCheckDuplicateMarkers() {
		return false;
	}

	protected void cleanSavedMarkers() {
		markers.clear();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String, java.lang.String, java.lang.String[], int, int, org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, String preferenceKey,
			String[] messageArguments, int lineNumber, int length, int offset, IResource target) {
		String preferenceValue = getPreference(target.getProject(), preferenceKey);
		IMarker marker = null;
		if (!SeverityPreferences.IGNORE.equals(preferenceValue)) {
			int severity = IMessage.HIGH_SEVERITY;
			if (SeverityPreferences.WARNING.equals(preferenceValue)) {
				severity = IMessage.NORMAL_SEVERITY;
			}
			if(shouldCheckDuplicateMarkers()) {
				MarkerID id = new MarkerID(preferenceKey, length, offset, target.getFullPath().toOSString());
				if(!markers.contains(id)) {
					marker = addError(message, severity, messageArguments, lineNumber, length, offset, target, getDocumentProvider(), getMarkerId(), getMarkerOwner());
					if(marker!=null) {
						markers.add(id);
					}
				}
			} else {
				marker = addError(message, severity, messageArguments, lineNumber, length, offset, target, getDocumentProvider(), getMarkerId(), getMarkerOwner());
			}
		}
		return marker;
	}

	public IMarker addError(String message, String preferenceKey,
			String[] messageArguments, int length, int offset, IResource target) {
		return addError(message, preferenceKey, messageArguments, 0, length, offset, target);
	}

	public TextFileDocumentProvider getDocumentProvider() {
		if(documentProvider==null) {
			if(coreHelper!=null) {
				documentProvider = coreHelper.getDocumentProvider();
			} else {
				documentProvider = new TextFileDocumentProvider();
			}
		}
		return documentProvider;
	}

	protected Class getMarkerOwner() {
		return this.getClass();
	}

	public IProjectValidationContext getValidationContext() {
		return validationContext;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String, int, java.lang.String[], int, int, org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, int severity, String[] messageArguments, int lineNumber, int length, int offset, IResource target) {
		return addError(message, severity, messageArguments, lineNumber, length, offset, target, getDocumentProvider(), getMarkerId(), getMarkerOwner());
	}

	public static IMarker addError(String message, int severity, Object[] messageArguments, int lineNumber, int length, int offset, IResource target, TextFileDocumentProvider documentProvider, String markerId, Class markerOwner, int maxNumberOfMarkersPerFile, String markerType) {
		IMarker marker = null;
		boolean connected = false;
		try {
			if(lineNumber<1) {
				if (documentProvider != null) {
					connected = true;
					documentProvider.connect(target);
					IDocument doc = documentProvider.getDocument(target);
					if(doc != null){
						try {
							lineNumber = doc.getLineOfOffset(offset) + 1;
						} catch (BadLocationException e) {
							CommonPlugin.getDefault().logError("Wrong offset [" + offset + "] of the problem marker [" + MessageFormat.format(message, messageArguments)  + "] for resource: " + target.getFullPath().toOSString(), e);  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
						}
					}
				}
			}
			marker = addTask(markerOwner.getName().intern(), target, lineNumber,
					MessageFormat.format(message, messageArguments),
					severity, null, markerId, offset, length, maxNumberOfMarkersPerFile, markerType);
		} catch (CoreException e) {
			CommonPlugin.getDefault().logError(
					NLS.bind(ValidationMessages.EXCEPTION_DURING_CREATING_MARKER, target.getFullPath()), e);
		} finally {
			if (documentProvider != null && connected) {
				documentProvider.disconnect(target);
			}
		}

		return marker;
	}

	/**
	 * Create a problem marker and add to the marker an attribute with the message ID for QuickFix.
	 * 
	 * @param message
	 * @param preferenceKey
	 * @param location
	 * @param target
	 * @param messageId
	 * @return
	 */
	public IMarker addError(String message, String preferenceKey,
			ITextSourceReference location, IResource target, int messageId) {
		IMarker marker = addError(message, preferenceKey, location, target);
		try {
			if(marker!=null) {
				marker.setAttribute(messageIdQuickFixAttributeName, new Integer(messageId));
			}
		} catch(CoreException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return marker;
	}

	/**
	 * 
	 * @param message
	 * @param severity
	 * @param messageArguments
	 * @param length
	 * @param offset
	 * @param target
	 * @param documentProvider
	 * @param markerId
	 * @param markerOwner
	 * @return
	 */
	public IMarker addError(String message, int severity, Object[] messageArguments, int lineNumber, int length, int offset, IResource target, TextFileDocumentProvider documentProvider, String markerId, Class markerOwner) {
		return addError(message, severity, messageArguments, lineNumber, length, offset, target, documentProvider, markerId, markerOwner, getMaxNumberOfMarkersPerFile(target.getProject()), getMarkerType());
	}

	abstract public int getMaxNumberOfMarkersPerFile(IProject project);

	public abstract String getMarkerType();

	private static IMarker addTask(String pluginId, IResource resource, int location, 
			String message, int severityEnumValue, String targetObjectName, 
			String groupName, int offset, int length, int maxNumberOfMarkersPerFile, String markerType) throws CoreException {

		int severity = getSeverity(severityEnumValue);

		if(markerType==null) {
			markerType = DEFAULT_VALIDATION_MARKER;
		}
		if(maxNumberOfMarkersPerFile>0) {
			int existingMarkers = resource.findMarkers(markerType, true, IResource.DEPTH_ZERO).length;
			if(existingMarkers>maxNumberOfMarkersPerFile) {
				return null;
			}
		}

		IMarker item = resource.createMarker(markerType); // add a validation marker

		boolean offsetSet = ((offset != IMessage.OFFSET_UNSET) && (length != IMessage.OFFSET_UNSET));
		int size = (offsetSet) ? 7 : 5;
		String[] attribNames = new String[size];
		Object[] attribValues = new Object[size];

		attribNames[0] = VALIDATION_MARKER_OWNER;
		attribValues[0] = pluginId;
		attribNames[1] = VALIDATION_MARKER_GROUP;
		attribValues[1] = ((groupName == null) ? "" : groupName); //$NON-NLS-1$
		attribNames[2] = IMarker.MESSAGE;
		attribValues[2] = message;
		attribNames[3] = IMarker.SEVERITY;
		attribValues[3] = new Integer(severity);

		Integer lineNumber = Integer.valueOf(location);
		attribNames[4] = IMarker.LINE_NUMBER;
		attribValues[4] = lineNumber;

		if (offsetSet) {
			attribNames[5] = IMarker.CHAR_START;
			attribValues[5] = new Integer(offset);
			attribNames[6] = IMarker.CHAR_END;
			attribValues[6] = new Integer(offset + length);
		}

		item.setAttributes(attribNames, attribValues);

		return item;
	}

	private static int getSeverity(int severityEnumValue) {
		switch (severityEnumValue) {
			case (IMessage.HIGH_SEVERITY) : {
				return IMarker.SEVERITY_ERROR;
			}
			case (IMessage.LOW_SEVERITY) : {
				return IMarker.SEVERITY_INFO;
			}
			case (IMessage.NORMAL_SEVERITY) : {
				return IMarker.SEVERITY_WARNING;
			}
			case (IMessage.ALL_MESSAGES) :
			case (IMessage.ERROR_AND_WARNING) :
			default : {
				// assume it's a warning.
				return IMarker.SEVERITY_WARNING;
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#displaySubtask(java.lang.String)
	 */
	public void displaySubtask(String messageId) {
		displaySubtask(messageId, null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#displaySubtask(java.lang.String,
	 *      java.lang.String[])
	 */
	public void displaySubtask(String message, String[] messageArguments) {
		IMessage problemMessage = new ProblemMessage(message, IMessage.NORMAL_SEVERITY, messageArguments);
		reporter.displaySubtask(validationManager, problemMessage);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#removeMessagesFromResources(java.util.Set)
	 */
	public void removeMessagesFromResources(Set<IResource> resources) {
		for (IResource r : resources) {
			WorkbenchReporter.removeAllMessages(r, new String[]{getMarkerOwner().getName()}, null);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#removeAllMessagesFromResource(org.eclipse.core.resources.IResource)
	 */
	public void removeAllMessagesFromResource(IResource resource) {
//		reporter.removeAllMessages(validationManager, resource);
		WorkbenchReporter.removeAllMessages(resource, new String[]{getMarkerOwner().getName()}, null);
	}
}