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

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.compiler.CategorizedProblem;
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitDocumentProvider.ProblemAnnotation;
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
final class JavaDirtyRegionProcessor extends
			DirtyRegionProcessor {

	/**
	 * This constant is put as value to JavaELProblemReporter.fAnnotations(Annotation, Position)
	 * to indicate that in JavaELProblemReporter.clearAnnotations(int start, int end)
	 * that annotation should be removed without regard to its position.
	 */
	static final Position ALWAYS_CLEARED = new Position(Integer.MAX_VALUE);

	private ITextEditor fEditor;
	private IDocument fDocument;
	private IValidationContext fHelper;
	private JavaELProblemReporter fReporter;
	private AsYouTypeValidatorManager fValidatorManager;

	private boolean fIsCanceled = false;
	private boolean fInRewriteSession = false;
	private IDocumentRewriteSessionListener fDocumentRewriteSessionListener = new DocumentRewriteSessionListener();
	private Set<ITypedRegion> fPartitionsToProcess = new HashSet<ITypedRegion>();
	private int fStartPartitionsToProcess = -1;
	private int fEndPartitionsToProcess = -1;

	// AsYouType EL Validation 'marker type' name. 
	// marker type is used in the quickFixProcessor extension point
	public static final String MARKER_TYPE= "org.jboss.tools.common.validation.el"; //$NON-NLS-1$

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

		Map<Annotation, Position> fAnnotations = new HashMap<Annotation, Position>();

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

		private void clearAllAnnotations() {
			if (fAnnotations.isEmpty()) {
				return;
			}
			Annotation[] annotations = fAnnotations.keySet().toArray(new Annotation[0]);
			for (Annotation annotation : annotations) {
				fAnnotations.remove(annotation);
				if(fAnnotationModel != null)
					fAnnotationModel.removeAnnotation(annotation);
			}
		}

		/**
		 * This method removes from annotation model each annotation stored 
		 * in JavaELProblemReporter.fAnnotations(Annotation, Position) that
		 * 1) either has position inside [start,end] region;
		 * 2) or is associated with special Position instance ALWAYS_CLEARED
		 * that indicates it should be removed without regard to its actual position.
		 */
		public void clearAnnotations(int start, int end) {
			if (fAnnotations.isEmpty()) {
				return;
			}
			Annotation[] annotations = fAnnotations.keySet().toArray(new Annotation[0]);
			for (Annotation annotation : annotations) {
				Position position = fAnnotations.get(annotation);
				if (position == ALWAYS_CLEARED || (position.getOffset() >= start && 
						position.getOffset() < end)) {
					// remove annotation from managed annotations map as well as from the model
					fAnnotations.remove(annotation);
					getAnnotationModel().removeAnnotation(annotation);
				}
			}
		}
	
		/**
		 * Adds annotation to the annotation model, and stores it in fAnnotations with either actual position
		 * or special constant ALWAYS_CLEARED (when cleanAllAnnotations = true) that indicates that 
		 * the annotation should be removed without regard to the modified region. 
		 * 
		 * @param annotation
		 * @param position
		 * @param cleanAllAnnotations
		 */
		public void addAnnotation(Annotation annotation, Position position, boolean cleanAllAnnotations) {
			if (isCancelled()) {
				return;
			}
			fAnnotations.put(annotation, cleanAllAnnotations ? ALWAYS_CLEARED : position);
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
					CoreELProblem problem= new CoreELProblem(valMessage, 
							editorInput.getName());
					if (fCompilationUnit != null) {
						ProblemAnnotation problemAnnotation = new ProblemAnnotation(problem, fCompilationUnit);
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

	class CoreELProblem extends CategorizedProblem {
		/** The end offset of the problem */
		private int fSourceEnd= 0;

		/** The line number of the problem */
		private int fLineNumber= 1;

		/** The start offset of the problem */
		private int fSourceStart= 0;

		/** The description of the problem */
		private String fMessage;

		private boolean fIsError;

		/** The originating file name */
		private String fOrigin;

		public static final int EL_PROBLEM_ID= 0x88000000;

		/**
		 * Initialize with the given parameters.
		 *
		 * @param message ValidationMessage
		 * @param document the document
		 * @param origin the originating file name
		 */
		public CoreELProblem(ValidationMessage message, String origin) {
			super();
			fSourceStart= message.getOffset();
			fSourceEnd= message.getOffset() + message.getLength() - 1;
			fLineNumber= message.getLineNumber();
			fMessage= message.getText();
			fOrigin= origin;
			fIsError = (IMessage.NORMAL_SEVERITY != message.getSeverity());
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getArguments()
		 */
		public String[] getArguments() {
			return new String[0];
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getID()
		 */
		public int getID() {
			return EL_PROBLEM_ID;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getMessage()
		 */
		public String getMessage() {
			return fMessage;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getOriginatingFileName()
		 */
		public char[] getOriginatingFileName() {
			return fOrigin.toCharArray();
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceEnd()
		 */
		public int getSourceEnd() {
			return fSourceEnd;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceLineNumber()
		 */
		public int getSourceLineNumber() {
			return fLineNumber;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#getSourceStart()
		 */
		public int getSourceStart() {
			return fSourceStart;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#isError()
		 */
		public boolean isError() {
			return fIsError;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#isWarning()
		 */
		public boolean isWarning() {
			return !fIsError;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceStart(int)
		 */
		public void setSourceStart(int sourceStart) {
			fSourceStart= sourceStart;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceEnd(int)
		 */
		public void setSourceEnd(int sourceEnd) {
			fSourceEnd= sourceEnd;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.IProblem#setSourceLineNumber(int)
		 */
		public void setSourceLineNumber(int lineNumber) {
			fLineNumber= lineNumber;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.CategorizedProblem#getCategoryID()
		 */
		@Override
		public int getCategoryID() {
			return CAT_SYNTAX;
		}

		/*
		 * @see org.eclipse.jdt.core.compiler.CategorizedProblem#getMarkerType()
		 */
		@Override
		public String getMarkerType() {
			return MARKER_TYPE;
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
	}

	@Override
	public void install(ITextViewer textViewer) {
		super.install(textViewer);
	}

	@Override
	public void uninstall() {
		fIsCanceled = true;
		if(fReporter != null) {
			fReporter.setCanceled(true);
		}

		super.uninstall();
	}

	@Override
	protected void beginProcessing() {
		fPartitionsToProcess.clear();
		fStartPartitionsToProcess = -1;
		fEndPartitionsToProcess = -1;
	}

	protected void process(DirtyRegion dirtyRegion) {
		IDocument doc = getDocument();
		if (!fEditor.isDirty() || !isInstalled() || isInRewrite() || dirtyRegion == null || doc == null || fIsCanceled) {
			return;
		}

		int start = dirtyRegion.getOffset();
		int end = dirtyRegion.getOffset() + dirtyRegion.getLength();

		// Check the document boundaries 
		int docLen = doc.getLength();
		if (docLen == 0)
			return;
		
		if (start > docLen)
			start = docLen;
		if (end >= docLen) 
			end = docLen - 1;
		
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

		if (fReporter != null) {
			fReporter.clearAnnotations(start, end);
		}
		for (ITypedRegion partition : partitions) {
			if (partition != null && !fIsCanceled && IJavaPartitions.JAVA_STRING.equals(partition.getType()) && !fPartitionsToProcess.contains(partition)) {
				fPartitionsToProcess.add(partition);
			}
		}
	}

	@Override
	protected void endProcessing() {
		if (fValidatorManager == null || fPartitionsToProcess.isEmpty() || fStartPartitionsToProcess == -1 || fEndPartitionsToProcess == -1) 
			return;
		
		if (fReporter != null) {
			fReporter.clearAnnotations(fStartPartitionsToProcess, fEndPartitionsToProcess);
		}
		for (ITypedRegion partition : fPartitionsToProcess) {
			fValidatorManager.validate(partition, fHelper, fReporter);
		}
	}
}