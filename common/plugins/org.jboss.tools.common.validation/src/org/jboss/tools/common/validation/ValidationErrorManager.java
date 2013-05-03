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

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.osgi.util.NLS;
import org.eclipse.ui.editors.text.TextFileDocumentProvider;
import org.eclipse.wst.validation.internal.MarkerManager;
import org.eclipse.wst.validation.internal.TaskListUtility;
import org.eclipse.wst.validation.internal.operations.WorkbenchReporter;
import org.eclipse.wst.validation.internal.plugin.ValidationPlugin;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.java.IJavaSourceReference;
import org.jboss.tools.common.preferences.SeverityPreferences;
import org.jboss.tools.common.text.ITextSourceReference;
import org.jboss.tools.common.util.EclipseJavaUtil;
import org.jboss.tools.common.util.EclipseUIUtil;

/**
 * @author Alexey Kazakov
 */
public abstract class ValidationErrorManager implements IValidationErrorManager {

	public static String DEFAULT_VALIDATION_MARKER = ValidationPlugin.PLUGIN_ID + ".problemmarker"; //$NON-NLS-1$
	static String VALIDATION_MARKER_OWNER = "owner"; //$NON-NLS-1$
	static String VALIDATION_MARKER_GROUP = "groupName"; //$NON-NLS-1$
	public static final String PREFERENCE_KEY_ATTRIBUTE_NAME = "preference_key"; //$NON-NLS-1$
	public static final String PREFERENCE_PAGE_ID_NAME = "preference_page_id"; //$NON-NLS-1$
	public static final String MESSAGE_ID_ATTRIBUTE_NAME = "Message_id"; //$NON-NLS-1$

	protected IStatus OK_STATUS = new Status(IStatus.OK, "org.eclipse.wst.validation", 0, "OK", null); //$NON-NLS-1$ //$NON-NLS-2$

	protected IValidator validationManager;
	protected ContextValidationHelper coreHelper;
	protected IReporter reporter;
	protected IProject validatingProject;
	protected String markerId;
	protected IProjectValidationContext validationContext;
	protected TextFileDocumentProvider documentProvider;
	protected IDocument document;
	protected Set<IFile> dirtyFiles;

	/**
	 * Constructor
	 */
	public ValidationErrorManager() {
	}

//	/**
//	 * @param messageIdQuickFixAttributeName the messageIdQuickFixAttributeName to set
//	 */
//	public void setMessageIdQuickFixAttributeName(
//			String messageIdQuickFixAttributeName) {
//		this.messageIdQuickFixAttributeName = messageIdQuickFixAttributeName;
//	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.validation.IValidationErrorManager#init(org.eclipse.core.resources.IProject, org.jboss.tools.common.validation.ContextValidationHelper, org.jboss.tools.common.validation.IProjectValidationContext, org.eclipse.wst.validation.internal.provisional.core.IValidator, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void init(IProject project, ContextValidationHelper validationHelper, IProjectValidationContext validationContext, IValidator manager, IReporter reporter) {
		init(project, validationHelper, validationContext, manager, reporter, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.jst.web.kb.validation.IValidationErrorManager#init(org.eclipse.core.resources.IProject, org.jboss.tools.jst.web.kb.internal.validation.ContextValidationHelper, org.jboss.tools.jst.web.kb.validation.IProjectValidationContext, org.eclipse.wst.validation.internal.provisional.core.IValidator, org.eclipse.wst.validation.internal.provisional.core.IReporter)
	 */
	public void init(IProject project, ContextValidationHelper validationHelper, IProjectValidationContext validationContext, IValidator manager, IReporter reporter, boolean asYouTypeValidation) {
		cleanSavedMarkers();
		setProject(project);
		setCoreHelper(validationHelper);
		setValidationManager(manager);
		setReporter(reporter);
		setValidationContext(validationContext);
		setMarkerId(org.jboss.tools.common.validation.IValidator.MARKED_RESOURCE_MESSAGE_GROUP);
		dirtyFiles = asYouTypeValidation?new HashSet<IFile>():EclipseUIUtil.getDirtyFiles();
	}

	protected boolean shouldBeValidated(IFile file) {
		return file.isAccessible() && !dirtyFiles.contains(file);
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
		IMarker marker = null;
		int severity = getSeverity(preferenceKey, newTarget);
		try {
			if(severity!=-1 && (severity!=IMessage.NORMAL_SEVERITY || !hasSuppressWarningsAnnotation(preferenceKey, location))) {
				marker = addError(message, preferenceKey, messageArguments, 0, location.getLength(), location.getStartPosition(), newTarget, severity);
			}
		} catch (JavaModelException e) {
			CommonPlugin.getDefault().logError(e);
		}

		return marker;
	}

	private static final String SUPPRESS_WARNINGS_ANNOTATION_SHORT = "SuppressWarnings";
	private static final String SUPPRESS_WARNINGS_ANNOTATION_FULL = "java.lang.SuppressWarnings";
	private static final String ALL_WARNINGS = "all";

	private static boolean hasSuppressWarningsAnnotation(String preferenceKey, IJavaElement element) throws JavaModelException {
		String[] names = WarningNameManager.getInstance().getWarningNames(preferenceKey);
		if(names==null || names.length==1) {
			return false;
		}
		// Does the element have @SuppressWarnings? Check it by the short name only.
		Set<IAnnotation> annotations = EclipseJavaUtil.findAnnotationsByShortName(element, SUPPRESS_WARNINGS_ANNOTATION_SHORT, true);
		if(annotations!=null) {
			for (IAnnotation annotation : annotations) {
				IMemberValuePair[] pairs = annotation.getMemberValuePairs();
				if(pairs.length==1) {
					Object v = pairs[0].getValue();
					Object[] warnings = null;
					if(v instanceof Object[]) {
						warnings = (Object[])v;
					} else if(v instanceof String) {
						warnings = new String[]{v.toString()};
					} else {
						continue;
					}
					for (Object warning : warnings) {
						if(warning == null) {
							continue;
						}
						boolean found = false;
						if(warning.equals(ALL_WARNINGS)) {
							found = true;
						} else {
							for (String name : names) {
								if(warning.equals(name)) {
									found = true;
									break;
								}
							}
						}
						if(found) {
							// Ok, we seem to have such a suppress. Let's make sure the full name of annotation is java.lang.SuppressWarnings
							if(EclipseJavaUtil.checkAnnotationByFulltName(annotation, SUPPRESS_WARNINGS_ANNOTATION_FULL)) {
								return true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	protected static boolean hasSuppressWarningsAnnotation(String preferenceKey, ITextSourceReference location) throws JavaModelException {
		boolean result = false;
		if(location instanceof IJavaSourceReference) {
			IJavaElement element = ((IJavaSourceReference) location).getSourceElement();
			if(element!=null) {
				result = hasSuppressWarningsAnnotation(preferenceKey, element);
			} else {
				// Check if it's really a java resource. 
				IResource resource = location.getResource();
				if("java".equalsIgnoreCase(resource.getFileExtension())) {
					throw new NullPointerException("IJavaSourceReference referenced to java source should not return null in getSourceElement()");
				}
			}
		}

		return result;
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
	
	protected int getSeverity(String preferenceKey, IResource target) {
		String preferenceValue = getPreference(target.getProject(), preferenceKey);
		int severity = -1;
		if (!SeverityPreferences.IGNORE.equals(preferenceValue)) {
			severity = SeverityPreferences.WARNING.equals(preferenceValue)?severity = IMessage.NORMAL_SEVERITY:IMessage.HIGH_SEVERITY;
		}
		return severity;
	}

	public IMarker addError(String message, String preferenceKey, String[] messageArguments, int lineNumber, int length, int offset, IResource target, int severity) {
		IMarker marker = null;
		if (severity!=-1) {
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
		try {
			if(marker!=null) {
				if(preferenceKey != null){
					marker.setAttribute(PREFERENCE_KEY_ATTRIBUTE_NAME, preferenceKey);
				}
			}
		} catch(CoreException e) {
			CommonPlugin.getDefault().logError(e);
		}
		return marker;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.seam.internal.core.validation.IValidationErrorManager#addError(java.lang.String, java.lang.String, java.lang.String[], int, int, org.eclipse.core.resources.IResource)
	 */
	public IMarker addError(String message, String preferenceKey, String[] messageArguments, int lineNumber, int length, int offset, IResource target) {
		return addError(message, preferenceKey, messageArguments, lineNumber, length, offset, target, getSeverity(preferenceKey, target));
	}
	
	public IMarker addError(String message, String preferenceKey,
			String[] messageArguments, int length, int offset, IResource target, int messageId) {
		IMarker marker = addError(message, preferenceKey, messageArguments, length, offset, target);
		try {
			if(marker!=null) {
				marker.setAttribute(MESSAGE_ID_ATTRIBUTE_NAME, new Integer(messageId));
			}
		} catch(CoreException e) {
			CommonPlugin.getDefault().logError(e);
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
					if(doc != null) {
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
				marker.setAttribute(MESSAGE_ID_ATTRIBUTE_NAME, new Integer(messageId));
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
		if(document != null && lineNumber < 0) {
			try {
				lineNumber = document.getLineOfOffset(offset) + 1;
			} catch (BadLocationException e) {
				CommonPlugin.getDefault().logError("Wrong offset [" + offset + "] of the problem marker [" + MessageFormat.format(message, messageArguments)  + "] for resource: " + target.getFullPath().toOSString(), e);  //$NON-NLS-1$ //$NON-NLS-2$//$NON-NLS-3$
			}
		}
		MarkerManager.getDefault().getMarkers().add(getProblemType()); // We need to register the marker type in WST since this type is not equaled to ValidatorManager's type. WST need this type to remove markers when the validator is disabled. See https://issues.jboss.org/browse/JBIDE-12595
		return addError(message, severity, messageArguments, lineNumber, length, offset, target, documentProvider, markerId, markerOwner, getMaxNumberOfMarkersPerFile(target.getProject()), getProblemType());
	}

	abstract public int getMaxNumberOfMarkersPerFile(IProject project);

	private static IMarker addTask(String pluginId, IResource resource, int location, 
			String message, int severityEnumValue, String targetObjectName, 
			String groupName, int offset, int length, int maxNumberOfMarkersPerFile, String markerType) throws CoreException {

		int severity = getSeverity(severityEnumValue);

		if(markerType==null) {
			markerType = DEFAULT_VALIDATION_MARKER;
		}
		if(maxNumberOfMarkersPerFile>0) {
			int existingMarkers = resource.findMarkers(markerType, true, IResource.DEPTH_ZERO).length;
			if(existingMarkers>=maxNumberOfMarkersPerFile) {
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

	/**
	 * Removes markers from the project. Doesn't remove markers from child resources.
	 */
	public void removeAllMessagesFromProject(IProject project) {
		Set<IMarker> markersToRemove = new HashSet<IMarker>();
		try {
			IMarker[] allMarkers = project.findMarkers(DEFAULT_VALIDATION_MARKER, true, IResource.DEPTH_ZERO);
			for (IMarker marker : allMarkers) {
				Object owner = marker.getAttribute(VALIDATION_MARKER_OWNER);
				if(getMarkerOwner().getName().equals(owner)) {
					markersToRemove.add(marker);
				}
			}
			ResourcesPlugin.getWorkspace().deleteMarkers(markersToRemove.toArray(new IMarker[markersToRemove.size()]));
		} catch (CoreException e) {
			CommonPlugin.getDefault().logError(e);
		}
	}

	/*
	 * register IPreferenceInfo in PreferenceInfoManager
	 * validator is supposed to have own implementation of IPreferenceInfo
	 * and register it in PreferenceInfoManager
	 * see CDICoreValidator.registerPreferenceInfo() as an example
	 */
	protected abstract void registerPreferenceInfo();
	
	protected String problemType = null;
	
	/**
	 * Sets type of problem for problem markers and problem annotations
	 * @param problemType
	 */
	public final void setProblemType(String problemType){
		this.problemType = problemType;  
	}
	
	/**
	 * @return type of problem for problem markers and problem annotations
	 */
	public final String getProblemType(){
		return problemType;
	}
}