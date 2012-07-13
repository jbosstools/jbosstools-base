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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.ui.text.IJavaPartitions;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.BadPartitioningException;
import org.eclipse.jface.text.DocumentRewriteSessionEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentExtension4;
import org.eclipse.jface.text.IDocumentRewriteSessionListener;
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
import org.eclipse.wst.sse.ui.internal.reconcile.DirtyRegionProcessor;
import org.eclipse.wst.validation.internal.provisional.core.IMessage;
import org.eclipse.wst.validation.internal.provisional.core.IReporter;
import org.eclipse.wst.validation.internal.provisional.core.IValidationContext;
import org.eclipse.wst.validation.internal.provisional.core.IValidator;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.log.LogHelper;
import org.jboss.tools.common.validation.AsYouTypeValidatorManager;
import org.jboss.tools.common.validation.CommonValidationPlugin;
import org.jboss.tools.common.validation.TempMarkerManager;
import org.jboss.tools.common.validation.ValidationMessage;

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
	private JavaELProblemReporter fReporter;
	private AsYouTypeValidatorManager fValidatorManager;

	private boolean fDocumentJustSetup = false;
	private boolean fIsCanceled = false;
	private boolean fInRewriteSession = false;
	private IDocumentRewriteSessionListener fDocumentRewriteSessionListener = new DocumentRewriteSessionListener();
	private Set<ITypedRegion> fPartitionsToProcess = new HashSet<ITypedRegion>();
	private int fStartPartitionsToProcess = -1;
	private int fEndPartitionsToProcess = -1;
	private int fStartRegionToProcess = -1;
	private int fEndRegionToProcess = -1;

	public final class JavaELProblemReporter implements IReporter {
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
			return null;
		}

		@Override
		public void displaySubtask(IValidator validator, IMessage message) {
			// Does nothing
		}

		Set<Annotation> fAnnotations = new HashSet<Annotation>();
		
		/**
		 * This set contains annotations that should be removed regardless to their positions
		 */
		Set<Annotation> fAlwaysRemoveAnnotations = new HashSet<Annotation>();

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
				if (fAlwaysRemoveAnnotations.contains(annotation))
					fAlwaysRemoveAnnotations.remove(annotation);
				if(fAnnotationModel != null)
					fAnnotationModel.removeAnnotation(annotation);
			}
		}

		/**
		 * This method removes from annotation model each annotation stored 
		 * in JavaELProblemReporter.fAnnotations(Annotation, Position) that
		 * 1) either has position inside [start,end] region;
		 * 2) or exists in fAlwaysRemoveAnnotations
		 * that indicates it should be removed without regard to its actual position.
		 */
		public void clearAnnotations(int start, int end) {
			if (fAnnotations.isEmpty()) {
				return;
			}
			Annotation[] annotations = fAnnotations.toArray(new Annotation[0]);
			for (Annotation annotation : annotations) {
				Position position = getAnnotationModel().getPosition(annotation);
				if (fAlwaysRemoveAnnotations.contains(annotation) || ( position != null && position.getOffset() >= start && 
						position.getOffset() <= end)) {
					// remove annotation from managed annotations map as well as from the model
					fAnnotations.remove(annotation);
					if (fAlwaysRemoveAnnotations.contains(annotation))
						fAlwaysRemoveAnnotations.remove(annotation);
					getAnnotationModel().removeAnnotation(annotation);
				}
			}
		}
	
		/**
		 * Adds annotation to the annotation model, and stores it in fAnnotations (when cleanAllAnnotations = true, that indicates that 
		 * the annotation should be removed without regard to the modified region, the annotation is stored in fAlwaysRemoveAnnotations as well). 
		 * 
		 * @param annotation
		 * @param position
		 * @param cleanAllAnnotations
		 */
		public void addAnnotation(Annotation annotation, Position position, boolean cleanAllAnnotations) {
			if (isCancelled()) {
				return;
			}
			fAnnotations.add(annotation);
			if (cleanAllAnnotations)
				fAlwaysRemoveAnnotations.add(annotation);
			fAnnotationModel.addAnnotation(annotation, position);
		}

		@Override
		public void addMessage(IValidator origin, IMessage message) {
			if (isCancelled()) {
				return;
			}
			if (message instanceof ValidationMessage && getAnnotationModel() != null) {
				ValidationMessage valMessage = (ValidationMessage)message;

				IEditorInput editorInput= fEditor.getEditorInput();
				if (editorInput != null) {
					boolean cleanAllAnnotations = Boolean.TRUE.equals(message.getAttribute(TempMarkerManager.CLEAN_ALL_ANNOTATIONS_ATTRIBUTE));
					Position position = new Position(valMessage.getOffset(), valMessage.getLength());
					TempJavaProblem problem = new TempJavaProblem(valMessage, 
							editorInput.getName());
					if (fCompilationUnit != null) {
						TempJavaProblemAnnotation problemAnnotation = new TempJavaProblemAnnotation(problem, fCompilationUnit);
						addAnnotation(problemAnnotation, position, cleanAllAnnotations);
					}
				}
			}
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

	private JavaELProblemReporter createProblemReporter() {
		JavaELProblemReporter reporter = new JavaELProblemReporter();
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
				if (IJavaPartitions.JAVA_STRING.equals(partition.getType()) && !fPartitionsToProcess.contains(partition)) {
					fPartitionsToProcess.add(partition);
				}
			}
		}
	}

	@Override
	protected void endProcessing() {
		if (fValidatorManager == null || fReporter == null || fStartPartitionsToProcess == -1 || fEndPartitionsToProcess == -1) 
			return;
		
		fReporter.clearAnnotations(fStartPartitionsToProcess, fEndPartitionsToProcess);

		for (ITypedRegion partition : fPartitionsToProcess) {
//			try {
//				System.out.println("validateString: " + partition.getOffset() + "->" + (partition.getOffset() + partition.getLength()) + ": [" + fDocument.get(partition.getOffset(), partition.getLength())+ "]");
//			} catch (BadLocationException e) {
//				e.printStackTrace();
//			}
			fValidatorManager.validateString(partition, fHelper, fReporter);
		}
		
		if (isJavaElementValidationRequired()) {
//			try {
//				System.out.println("validateJavaElement: " + fStartRegionToProcess + "->" + fEndRegionToProcess + ": [" + fDocument.get(fStartRegionToProcess, fEndRegionToProcess - fStartRegionToProcess)+ "]");
//			} catch (BadLocationException e) {
//				e.printStackTrace();
//			}
			fValidatorManager.validateJavaElement(new Region(fStartRegionToProcess, fEndRegionToProcess - fStartRegionToProcess), fHelper, fReporter);			
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

				element = unit.getElementAt(position++);
				if (element == null)
					continue;
				
				atLeastOneElementIsProcessed = true;
				if (element.getElementType() != IJavaElement.METHOD) {
					IJavaElement parent = element.getParent();
					boolean doSkipThisElement = false;
					while (parent != null && parent.getElementType() != IJavaElement.COMPILATION_UNIT) {
						if (parent.getElementType() == IJavaElement.METHOD) {
							doSkipThisElement = true;
							break;
						}
						parent = parent.getParent();
					}
					
					if (!doSkipThisElement) 
						return true;
				}
			}
		} catch (JavaModelException e) {
			LogHelper.logError(CommonValidationPlugin.getDefault(), e);
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