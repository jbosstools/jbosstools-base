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
package org.jboss.tools.common.validation.java;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.text.IJavaPartitions;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPartitioningException;
import org.eclipse.jface.text.DocumentRewriteSessionEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.IDocumentRewriteSessionListener;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.reconciler.DirtyRegion;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.log.LogHelper;
import org.jboss.tools.common.validation.AsYouTypeValidatorManager;
import org.jboss.tools.common.validation.CommonValidationPlugin;
import org.jboss.tools.common.validation.ITypedReporter;
import org.jboss.tools.common.validation.TempMarkerManager;
import org.jboss.tools.common.validation.ValidationMessage;
import org.jboss.tools.common.validation.java.xpl.DirtyRegionProcessor;

/**
 * As-You-Type validation Java files
 * 
 * @author Victor V. Rubezhny
 *
 */
@SuppressWarnings("restriction")
final public class JavaDirtyRegionProcessor extends
			DirtyRegionProcessor {

	private ITextEditor fEditor;
	private IDocument fDocument;
	private IValidationContext fHelper;
	private JavaProblemReporter fReporter;
	private AsYouTypeValidatorManager fValidatorManager;

	private boolean fDocumentJustSetup = false;
	private boolean fIsCanceled = false;
	private boolean fInRewriteSession = false;
	private IDocumentRewriteSessionListener fDocumentRewriteSessionListener = new DocumentRewriteSessionListener();
	private List<ITypedRegion> fPartitionsToProcess = new ArrayList<ITypedRegion>();
	private int fStartPartitionsToProcess = -1;
	private int fEndPartitionsToProcess = -1;
	private int fStartRegionToProcess = -1;
	private int fEndRegionToProcess = -1;

	public final class JavaProblemReporter implements IReporter, ITypedReporter {
		private List<IMessage> messages = new ArrayList<IMessage>();
		private IFile fFile;
		private ICompilationUnit fCompilationUnit;
		private IAnnotationModel fAnnotationModel;
		private boolean fIsCanceled = false;

		@Override
		public void removeMessageSubset(IValidator validator, Object obj, String groupName) {
			// Does nothing
		}

		@Override
		public void removeAllMessages(IValidator origin, Object object) {
			// Does nothing
		}

		@Override
		public void removeAllMessages(IValidator origin) {
			// Does nothing
		}

		public void removeAllMessages() {
			messages.clear();
		}

		public void setCanceled(boolean set) {
			this.fIsCanceled = set;
		}

		@Override
		public boolean isCancelled() {
			return this.fIsCanceled;
		}

		@SuppressWarnings("rawtypes")
		@Override
		public List getMessages() {
			return messages;
		}

		@Override
		public void displaySubtask(IValidator validator, IMessage message) {
			// Does nothing
		}

		Set<Annotation> fAnnotations = new HashSet<Annotation>();
		
		public void update() {
			clearAllAnnotations();
			getAnnotationModel(); // This updates saved annotation model if needed
			fFile = (fEditor != null && fEditor.getEditorInput() instanceof IFileEditorInput ? ((IFileEditorInput)fEditor.getEditorInput()).getFile() : null);
			fCompilationUnit = (fFile != null ? EclipseUtil.getCompilationUnit(fFile) : null);
		}

		protected IAnnotationModel getAnnotationModel() {
			final IDocumentProvider documentProvider= fEditor.getDocumentProvider();
			if (documentProvider == null) {
				return null;
			}
			IAnnotationModel newModel = documentProvider.getAnnotationModel(fEditor.getEditorInput());
			if (fAnnotationModel != newModel) {
				fAnnotationModel = newModel;
			}
			return fAnnotationModel;
		}

		public ICompilationUnit getCompilationUnit() {
			return fCompilationUnit;
		}
		
		public void clearAllAnnotations() {
			if (fAnnotations.isEmpty()) {
				return;
			}
			Annotation[] annotations = fAnnotations.toArray(new Annotation[0]);
			for (Annotation annotation : annotations) {
				fAnnotations.remove(annotation);
				if(fAnnotationModel != null)
					fAnnotationModel.removeAnnotation(annotation);
			}
		}

		@Override
		public void addMessage(IValidator origin, IMessage message) {
			messages.add(message);
		}
		
		public void finishReporting() {
			if (isCancelled() || getAnnotationModel() == null || fCompilationUnit == null)
				return;
			
			IEditorInput editorInput= fEditor.getEditorInput();
			if (editorInput == null)
				return;
			
			String editorInputName = editorInput.getName();
			Collection<String> regionTypes = getTypesForRegion();
			Collection<String> fileTypes = getTypesForFile();
			
			Annotation[] annotations = fAnnotations.toArray(new Annotation[0]);
			List<Annotation> annotationsToRemove = new ArrayList<Annotation>();
			Set<ValidationMessage> existingValidationMessages = new HashSet<ValidationMessage>();
			
			for (Annotation annotation : annotations) {
				if (!(annotation instanceof TempJavaProblemAnnotation))
					continue;
				
				TempJavaProblemAnnotation jpAnnotation = (TempJavaProblemAnnotation)annotation;
				Position position = getAnnotationModel().getPosition(jpAnnotation);
				Map attributes = jpAnnotation.getAttributes();
				Object typeOfAnnotation = attributes == null ? null : attributes.get(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME);
				boolean isRegionWideAnnotationType = regionTypes.contains(typeOfAnnotation);
				boolean isFileWideAnnotationType = fileTypes.contains(typeOfAnnotation);
				IRegion regionOfAnnotation = position == null ? null : findRegion(position.getOffset());

				// Find a validation message for the annotation
				boolean existing = false;
				for (IMessage m : messages) {
					if (!(m instanceof ValidationMessage))
						continue;
					
					ValidationMessage valMessage = (ValidationMessage)m;
					if (position != null && position.getOffset() == valMessage.getOffset() && position.getLength() == valMessage.getLength()) {
						String text = valMessage.getText();
						text = text == null ? "" : text; //$NON-NLS-1$
						if (!text.equalsIgnoreCase(jpAnnotation.getText()))
							continue;
						
						Object type = valMessage.getAttribute(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME);
						type = type == null ? "" : type; //$NON-NLS-1$
						Map jpAttributes = jpAnnotation.getAttributes();
						if (jpAttributes == null)
							continue;
						
						if (!type.equals(jpAttributes.get(TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME)))
							continue;
						
						// This is an annotation to keep (message is found for the annotation)
						existingValidationMessages.add(valMessage);
						existing = true;
						break;
					}
				}

				// This is an annotation to remove (no message found for the annotation)
				if (!existing) {
					if (isFileWideAnnotationType || (isRegionWideAnnotationType && regionOfAnnotation != null))
						annotationsToRemove.add(annotation);
				}
			}
			
			Map<Annotation, Position> annotationsToAdd = new HashMap<Annotation, Position>();
			for (IMessage message : messages) {
				if (!(message instanceof ValidationMessage) || 
						existingValidationMessages.contains(message))
					continue;

				ValidationMessage valMessage = (ValidationMessage)message;
				Position position = new Position(valMessage.getOffset(), valMessage.getLength());
				TempJavaProblem problem = new TempJavaProblem(valMessage, editorInputName);
				TempJavaProblemAnnotation problemAnnotation = new TempJavaProblemAnnotation(problem, fCompilationUnit);
				annotationsToAdd.put(problemAnnotation, position);
			}

			getAnnotationModel(); // This is to update saved document annotation model 
			for (Annotation a : annotationsToRemove) {
				fAnnotations.remove(a);
				fAnnotationModel.removeAnnotation(a);
			}
			
			for (Annotation a : annotationsToAdd.keySet()) {
				Position p = annotationsToAdd.get(a);
				fAnnotations.add(a);
				fAnnotationModel.addAnnotation(a, p);
			}
			removeAllMessages();
			clearRegions();
		}

		private List<String> fTypesForFileValidation = new ArrayList<String>();
		private List<String> fTypesForRegionValidatoin = new ArrayList<String>();

		@Override
		public void addTypeForFile(String type) {
			if (!fTypesForFileValidation.contains(type)) {
				fTypesForFileValidation.add(type);
			}
		}

		@Override
		public Collection<String> getTypesForFile() {
			return Collections.unmodifiableList(fTypesForFileValidation);
		}

		@Override
		public void addTypeForRegion(String type) {
			if (!fTypesForRegionValidatoin.contains(type)) {
				fTypesForRegionValidatoin.add(type);
			}
		}

		@Override
		public Collection<String> getTypesForRegion() {
			return Collections.unmodifiableList(fTypesForRegionValidatoin);
		}
		
		private List<IRegion> fRegions;
		public void setRegions(List<IRegion> regions) {
			this.fRegions = regions;
		}
		
		public void clearRegions() {
			this.fRegions = null;
		}
		
		private IRegion findRegion(int position) {
			if (fRegions != null) {
				for (IRegion region : fRegions) {
					if (region.getOffset() <= position && region.getOffset() + region.getLength() > position)
						return region;
				}
			}
			
			return null;
		}
	}

	class DocumentRewriteSessionListener implements IDocumentRewriteSessionListener {
		public void documentRewriteSessionChanged(DocumentRewriteSessionEvent event) {
			fInRewriteSession = event != null && event.getChangeType().equals(DocumentRewriteSessionEvent.SESSION_START);
		}
	}
	
	public JavaDirtyRegionProcessor(ITextEditor editor) {
		this.fEditor = editor;
		fHelper = createValidationContext();
		fReporter = createProblemReporter();
	}

	private IValidationContext createValidationContext() {
		return new IValidationContext() {
			@Override
			public Object loadModel(String arg0, Object[] arg1) {
				return null;
			}

			@Override
			public Object loadModel(String arg0) {
				return null;
			}

			@Override
			public String[] getURIs() {
				IFile file = (fEditor != null && fEditor.getEditorInput() instanceof IFileEditorInput ? ((IFileEditorInput)fEditor.getEditorInput()).getFile() : null);
				String URI = file == null ? null : file.getFullPath().toPortableString();
				return URI == null ? new String[0] : new String[] {URI};
			}
		};
	}

	private JavaProblemReporter createProblemReporter() {
		JavaProblemReporter reporter = new JavaProblemReporter();
		reporter.update();
		return reporter; 
	}

	@Override
	public synchronized void startReconciling() {
		super.startReconciling();
	}

	private boolean isInRewrite() {
		return fInRewriteSession;
	}

	@Override
	public void setDocument(IDocument doc) {
		if (fDocument != null) {
			if (fDocument instanceof IDocumentExtension4) {
				((IDocumentExtension4) fDocument).removeDocumentRewriteSessionListener(fDocumentRewriteSessionListener);
			}
			if (fValidatorManager != null && fDocument != null) {
				fValidatorManager.disconnect(fDocument);
			}
		}

		fDocument = doc;
		super.setDocument(doc);

		if (fDocument != null) {
			if (fDocument instanceof IDocumentExtension4) {
				((IDocumentExtension4) fDocument).addDocumentRewriteSessionListener(fDocumentRewriteSessionListener);
			}
			if (fValidatorManager == null) {
				fValidatorManager = new AsYouTypeValidatorManager();
			}

			fValidatorManager.connect(fDocument);

			if (fReporter != null) {
				fReporter.update();
			}
		}
		fDocumentJustSetup = true;
	}

	@Override
	public void install(ITextViewer textViewer) {
		super.install(textViewer);
	}

	@Override
	public void uninstall() {
		fIsCanceled = true;
		if(fReporter != null) {
			fReporter.clearAllAnnotations();
			fReporter.setCanceled(true);
		}

		super.uninstall();
	}

	@Override
	protected void beginProcessing() {
		fPartitionsToProcess.clear();
		fStartRegionToProcess = -1;
		fEndRegionToProcess = -1;
		fStartPartitionsToProcess = -1;
		fEndPartitionsToProcess = -1;
	}

	private boolean isEditorDirty() {
		if (fDocumentJustSetup && fEditor.isDirty()) {
			fDocumentJustSetup = false;
		}
		
		return !fDocumentJustSetup;
	}

	protected void process(DirtyRegion dirtyRegion) {
		IDocument doc = getDocument();
				
		if (!isEditorDirty() || !isInstalled() || isInRewrite() || dirtyRegion == null || doc == null || fIsCanceled) {
			return;
		}

		int start = dirtyRegion.getOffset();
		int end = DirtyRegion.REMOVE.equals(dirtyRegion.getType()) ? dirtyRegion.getOffset() : dirtyRegion.getOffset() + dirtyRegion.getLength();

		// Check the document boundaries 
		int docLen = doc.getLength();
		if (docLen == 0)
			return;
		
		if (start > docLen)
			start = docLen;
		if (end >= docLen) 
			end = docLen - 1;

		fStartRegionToProcess = (fStartRegionToProcess == -1 || fStartRegionToProcess > start) ? start : fStartRegionToProcess;
		fEndRegionToProcess = (fEndRegionToProcess == -1 || fEndRegionToProcess < end) ? end : fEndRegionToProcess;
		
		/*
		 * Expand dirtyRegion to partitions boundaries 
		 */
		try {
			ITypedRegion startPartition = (doc instanceof IDocumentExtension3) ? 
					((IDocumentExtension3)doc).getPartition(IJavaPartitions.JAVA_PARTITIONING, start, true) :
						doc.getPartition(start);
			if (startPartition != null && start > startPartition.getOffset())
				start = startPartition.getOffset();
			
			ITypedRegion endPartition = (doc instanceof IDocumentExtension3) ? 
					((IDocumentExtension3)doc).getPartition(IJavaPartitions.JAVA_PARTITIONING, end, false) :
						doc.getPartition(end);
			if (endPartition != null && end < endPartition.getOffset() + endPartition.getLength())
				end = endPartition.getOffset() + endPartition.getLength();
		} catch (BadLocationException e) {
			LogHelper.logError(CommonValidationPlugin.getDefault(), e);
		} catch (BadPartitioningException e) {
			LogHelper.logError(CommonValidationPlugin.getDefault(), e);
		}

		fStartPartitionsToProcess = (fStartPartitionsToProcess == -1 || fStartPartitionsToProcess > start) ? start : fStartPartitionsToProcess;
		fEndPartitionsToProcess = (fEndPartitionsToProcess == -1 || fEndPartitionsToProcess < end) ? end : fEndPartitionsToProcess;

		ITypedRegion[] partitions = computePartitioning(start, end - start);
		for (ITypedRegion partition : partitions) {
			if (partition != null && !fIsCanceled) {
				String type = partition.getType();
				if ((IJavaPartitions.JAVA_STRING.equals(type) || IJavaPartitions.JAVA_CHARACTER.equals(type) ||
						IJavaPartitions.JAVA_SINGLE_LINE_COMMENT.equals(type) || 
						IJavaPartitions.JAVA_MULTI_LINE_COMMENT.equals(type) || IJavaPartitions.JAVA_DOC.equals(type)) 
						&& !fPartitionsToProcess.contains(partition)) {
					fPartitionsToProcess.add(partition);
				}
			}
		}
	}

	@Override
	protected void endProcessing() {
		if (fValidatorManager == null || fReporter == null || fStartPartitionsToProcess == -1 || fEndPartitionsToProcess == -1) 
			return;

		fReporter.clearRegions();
		if (fPartitionsToProcess != null && !fPartitionsToProcess.isEmpty()) {
			List<IRegion> regions = Arrays.asList(fPartitionsToProcess.toArray(new IRegion[fPartitionsToProcess.size()]));
			fReporter.setRegions(regions);
			fValidatorManager.validateString(
					regions, 
					fHelper, fReporter);
			fReporter.finishReporting();
		} else if (isJavaElementValidationRequired()) {
			// The 'else' is added here due to not to validate 
			// an element in case of at lease one string is validated,
			// because the string validation performs the validation of an element
			// as well
			fValidatorManager.validateJavaElement(
				Arrays.asList(
					new IRegion[] {
						new Region(fStartRegionToProcess, fEndRegionToProcess - fStartRegionToProcess)
					}), 
				fHelper, fReporter);
			fReporter.finishReporting();
		} 
	}

	private boolean isJavaElementValidationRequired() {
		ICompilationUnit unit = fReporter.getCompilationUnit();
		if (unit == null)
			return false;
		
		boolean result = false;
		boolean atLeastOneElementIsProcessed = false;
		
		int position = fStartRegionToProcess;
		try {
			unit = unit.getWorkingCopy(null);
			IJavaElement element = null;
			while (position >= 0 && (element = unit.getElementAt(position--)) == null)
				;
			
			if (position < 0)
				position = 0;

			ITypedRegion[] partitions = computePartitioning(position, fEndPartitionsToProcess - position);
			
			ITypedRegion startPartition = findPartitionByOffset(partitions, position);
			ITypedRegion endPartition = (startPartition != null && fEndRegionToProcess >= startPartition.getOffset() && 
					fEndRegionToProcess < startPartition.getOffset() + startPartition.getLength()) ?
					startPartition : findPartitionByOffset(partitions, fEndRegionToProcess);
			
			if (startPartition != null && startPartition.equals(endPartition) && !isProcessingRequiredForPartition(startPartition)) {
				return false;
			}

			while (position <= fEndRegionToProcess) {
				ITypedRegion partition = findPartitionByOffset(partitions, position);
				if(!isProcessingRequiredForPartition(partition)) {
					position = partition.getOffset() + partition.getLength();
					continue;
				}

				element = unit.getElementAt(position);
				if (element == null) {
					position++;
					continue;
				}
				
				atLeastOneElementIsProcessed = true;
				boolean doSkipThisElement = false;
				if (element instanceof IMember && element.getElementType() == IJavaElement.METHOD) { 
					ISourceRange range = ((IMember)element).getSourceRange();
					if (position >= range.getOffset()) {
						try {
							String text = fDocument.get(range.getOffset(), position - range.getOffset() + 1); 
							if (text.indexOf('{') != -1 && !text.endsWith("}")) { //$NON-NLS-1$
								doSkipThisElement = true;
								position = range.getOffset() + range.getLength();
							}
						} catch (BadLocationException e) {
							// Ignore it and do not skip validation
						}
						position++;
					}
				} else {
					IJavaElement parent = element.getParent();
					while (parent != null && parent.getElementType() != IJavaElement.COMPILATION_UNIT) {
						if (parent.getElementType() == IJavaElement.METHOD) {
							doSkipThisElement = true;
							break;
						}
						parent = parent.getParent();
					}
					position++;
				}
					
				if (!doSkipThisElement) 
					return true;
			}
		} catch (JavaModelException e) {
			LogHelper.logError(CommonValidationPlugin.getDefault(), e);
		} finally {
			try {
				unit.discardWorkingCopy();
			} catch (JavaModelException e) {
				LogHelper.logError(CommonValidationPlugin.getDefault(), e);
			}
		}
		
		return atLeastOneElementIsProcessed ? result : true;
	}
	
	private ITypedRegion findPartitionByOffset(ITypedRegion[] partitions, int offset) {
		if (partitions == null) 
			return null;
		
		for (ITypedRegion partition : partitions) {
			if (offset >= partition.getOffset() && offset < partition.getOffset() + partition.getLength())
				return partition;
		}
		
		return null;
	}
	
	private boolean isProcessingRequiredForPartition(ITypedRegion partition) {
		if (partition == null)
			return false;
		
		String type = partition.getType();
		return !(IJavaPartitions.JAVA_STRING.equals(type) || IJavaPartitions.JAVA_CHARACTER.equals(type) ||
				IJavaPartitions.JAVA_SINGLE_LINE_COMMENT.equals(type) || 
				IJavaPartitions.JAVA_MULTI_LINE_COMMENT.equals(type) || IJavaPartitions.JAVA_DOC.equals(type));
	}
	
}