/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc.
 *******************************************************************************/
package org.jboss.tools.jst.jsp.text.xpl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.ISelectionValidator;
import org.eclipse.jface.text.ISynchronizable;
import org.eclipse.jface.text.ITextInputListener;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.projection.ProjectionViewer;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion;
import org.eclipse.wst.sse.ui.StructuredTextEditor;
import org.eclipse.wst.xml.core.internal.document.NodeContainer;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMAttr;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMElement;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMText;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;
import org.jboss.tools.common.text.xml.ui.xpl.ISelectionListenerWithSM;
import org.jboss.tools.common.text.xml.ui.xpl.SelectionListenerWithSMManager;
import org.jboss.tools.common.text.xml.ui.xpl.UIMessages;
import org.jboss.tools.jst.jsp.preferences.xpl.PreferenceKeyGenerator;
import org.jboss.tools.jst.jsp.preferences.xpl.XMLOccurrencePreferenceConstants;
import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Jeremy
 *
 */
public class DefaultStructuredTextOccurrenceStructureProvider implements IStructuredTextOccurrenceStructureProvider {

	private OccurrencesFinderJob fOccurrencesFinderJob;
	/** The occcurrences finder job canceler */
	private OccurrencesFinderJobCanceler fOccurrencesFinderJobCanceler;
	private ISelectionListenerWithSM fPostSelectionListenerWithSM;
	private IPreferenceStore fPreferenceStore;
	private String fEditorID;
	
	public DefaultStructuredTextOccurrenceStructureProvider(String editorID, IPreferenceStore store) {
		this.fEditorID = editorID;
		this.fPreferenceStore = store;
	}
		
	/**
	 * Internal property change listener for handling changes in the editor's preferences.
	 */
	class PropertyChangeListener implements IPropertyChangeListener {
		/*
		 * @see IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
		 */
		public void propertyChange(PropertyChangeEvent event) {
			handlePreferenceStoreChanged(event);
		}
	}
	private IPropertyChangeListener fPropertyChangeListener= new PropertyChangeListener();

	/**
	 * Holds the current occurrence annotations.
	 * @since 3.0
	 */
	private Annotation[] fOccurrenceAnnotations= null;
	/**
	 * 
	 */
	private ISelection fForcedMarkOccurrencesSelection;
	
	/**
	 * Tells whether all occurrences of the element at the
	 * current caret location are automatically marked in
	 * this editor.
	 * @since 3.0
	 */
	private boolean fMarkOccurrenceAnnotations = true;
	/**
	 * Tells whether the occurrence annotations are sticky
	 * i.e. whether they stay even if there's no valid Java
	 * element at the current caret position.
	 * Only valid if {@link #fMarkOccurrenceAnnotations} is <code>true</code>.
	 * @since 3.0
	 */
	private boolean fStickyOccurrenceAnnotations;
	/**
	 * Tells whether to mark node occurrences in this editor.
	 * Only valid if {@link #fMarkOccurrenceAnnotations} is <code>true</code>.
	 * @since 3.0
	 */
	private boolean fMarkNodeOccurrences;
	/**
	 * Tells whether to mark attribute occurrences in this editor.
	 * Only valid if {@link #fMarkOccurrenceAnnotations} is <code>true</code>.
	 * @since 3.0
	 */
	private boolean fMarkAttributeOccurrences;
	/**
	 * Tells whether to mark attribute value occurrences in this editor.
	 * Only valid if {@link #fMarkOccurrenceAnnotations} is <code>true</code>.
	 * @since 3.0
	 */
	private boolean fMarkAttributeValueOccurrences;

	/**
	 * Tells whether to mark attribute value occurrences in this editor.
	 * Only valid if {@link #fMarkOccurrenceAnnotations} is <code>true</code>.
	 * @since 3.0
	 */
	private boolean fMarkTextOccurrences;

	private String fOccurrenceProvider;
	/**
	 * Finds and marks occurrence annotations.
	 * 
	 * @since 3.0
	 */
	protected class OccurrencesFinderJob extends Job {
		
		private IDocument fDocument;
		private ISelection fSelection;
		private ISelectionValidator fPostSelectionValidator;
		private boolean fCanceled= false;
		private IProgressMonitor fProgressMonitor;
		private Position[] fPositions;
		
		public OccurrencesFinderJob(IDocument document, Position[] positions, ISelection selection) {
			super(UIMessages.RedHatStructuredTextEditor_markOccurrences_job_name); 
			fDocument= document;
			fSelection= selection;
			fPositions= positions;
			
			if (fEditor.getSelectionProvider() instanceof ISelectionValidator)
				fPostSelectionValidator= (ISelectionValidator)fEditor.getSelectionProvider(); 
		}
		
		// cannot use cancel() because it is declared final
		void doCancel() {
			fCanceled= true;
			cancel();
		}
		
		private boolean isCanceled() {
			return fCanceled || fProgressMonitor.isCanceled()
				||  fPostSelectionValidator != null 
				&& !(fPostSelectionValidator.isValid(fSelection) 
				|| fForcedMarkOccurrencesSelection == fSelection)
				|| LinkedModeModel.hasInstalledModel(fDocument);
		}
		
		/*
		 * @see Job#run(org.eclipse.core.runtime.IProgressMonitor)
		 */
		public IStatus run(IProgressMonitor progressMonitor) {
			
			fProgressMonitor= progressMonitor;
			
			if (isCanceled())
				return Status.CANCEL_STATUS;
			
			ITextViewer textViewer= getSourceViewer(); 
			if (textViewer == null)
				return Status.CANCEL_STATUS;
			
			IDocument document= textViewer.getDocument();
			if (document == null)
				return Status.CANCEL_STATUS;
			
			IDocumentProvider documentProvider= fEditor.getDocumentProvider();
			if (documentProvider == null)
				return Status.CANCEL_STATUS;
		
			IAnnotationModel annotationModel= documentProvider.getAnnotationModel(fEditor.getEditorInput());
			if (annotationModel == null)
				return Status.CANCEL_STATUS;
			
			// Add occurrence annotations
			int length= fPositions.length;
			Map annotationMap= new HashMap(length);
			for (int i= 0; i < length; i++) {
				
				if (isCanceled())
					return Status.CANCEL_STATUS; 
				
				String message;
				Position position= fPositions[i];
				
				// Create & add annotation
				try {
					message= document.get(position.offset, position.length);
				} catch (BadLocationException ex) {
					XmlEditorPlugin.getPluginLog().logError(ex);
					// Skip this match
					continue;
				}
				annotationMap.put(
						new Annotation("org.jboss.tools.jst.jsp.occurrences", false, message), //$NON-NLS-1$
						position);
			}
			
			if (isCanceled())
				return Status.CANCEL_STATUS;
			
			synchronized (getLockObject(annotationModel)) {
				if (annotationModel instanceof IAnnotationModelExtension) {
					((IAnnotationModelExtension)annotationModel).replaceAnnotations(fOccurrenceAnnotations, annotationMap);
				} else {
					removeOccurrenceAnnotations();
					Iterator iter= annotationMap.entrySet().iterator();
					while (iter.hasNext()) {
						Map.Entry mapEntry= (Map.Entry)iter.next(); 
						annotationModel.addAnnotation((Annotation)mapEntry.getKey(), (Position)mapEntry.getValue());
					}
				}
				fOccurrenceAnnotations= (Annotation[])annotationMap.keySet().toArray(new Annotation[annotationMap.keySet().size()]);
			}

			return Status.OK_STATUS;
		}
	}	

	/**
	 * Returns the lock object for the given annotation model.
	 * 
	 * @param annotationModel the annotation model
	 * @return the annotation model's lock object
	 * @since 3.0
	 */
	public Object getLockObject(IAnnotationModel annotationModel) { 
		if (annotationModel instanceof ISynchronizable)
			return ((ISynchronizable)annotationModel).getLockObject();
		else
			return annotationModel;
	}

	/**
	 * Cancels the occurrences finder job upon document changes.
	 * 
	 * @since 3.0
	 */
	protected class OccurrencesFinderJobCanceler implements IDocumentListener, ITextInputListener {

		public void install() {
			ISourceViewer sourceViewer= getSourceViewer();
			if (sourceViewer == null)
				return;
				
			StyledText text= sourceViewer.getTextWidget();			
			if (text == null || text.isDisposed())
				return;

			sourceViewer.addTextInputListener(this);
			
			IDocument document= sourceViewer.getDocument();
			if (document != null)
				document.addDocumentListener(this);			
		}
		
		public void uninstall() {
			ISourceViewer sourceViewer= getSourceViewer();
			if (sourceViewer != null)
				sourceViewer.removeTextInputListener(this);

			IDocumentProvider documentProvider= fEditor.getDocumentProvider();
			if (documentProvider != null) {
				IDocument document= documentProvider.getDocument(fEditor.getEditorInput());
				if (document != null)
					document.removeDocumentListener(this);
			}
		}
				

		/*
		 * @see org.eclipse.jface.text.IDocumentListener#documentAboutToBeChanged(org.eclipse.jface.text.DocumentEvent)
		 */
		public void documentAboutToBeChanged(DocumentEvent event) {
			if (fOccurrencesFinderJob != null)
				fOccurrencesFinderJob.doCancel();
		}

		/*
		 * @see org.eclipse.jface.text.IDocumentListener#documentChanged(org.eclipse.jface.text.DocumentEvent)
		 */
		public void documentChanged(DocumentEvent event) {
		}

		/*
		 * @see org.eclipse.jface.text.ITextInputListener#inputDocumentAboutToBeChanged(org.eclipse.jface.text.IDocument, org.eclipse.jface.text.IDocument)
		 */
		public void inputDocumentAboutToBeChanged(IDocument oldInput, IDocument newInput) {
			if (oldInput == null)
				return;

			oldInput.removeDocumentListener(this);
		}

		/*
		 * @see org.eclipse.jface.text.ITextInputListener#inputDocumentChanged(org.eclipse.jface.text.IDocument, org.eclipse.jface.text.IDocument)
		 */
		public void inputDocumentChanged(IDocument oldInput, IDocument newInput) {
			if (newInput == null)
				return;
			newInput.addDocumentListener(this);
		}
	}

	private StructuredTextEditor fEditor;
	private ProjectionViewer fViewer;
	
	
	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.ui.text.IStructuredTextOccurenceStructureProvider#install(org.eclipse.ui.texteditor.ITextEditor, org.eclipse.jface.text.source.projection.ProjectionViewer)
	 */
	public void install(StructuredTextEditor editor, ProjectionViewer viewer) {
		this.fEditor = editor;
		this.fViewer = viewer;
		
		initialize();
		
		if (fMarkOccurrenceAnnotations)
			installOccurrencesFinder();
		
	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.ui.text.IStructuredTextOccurenceStructureProvider#uninstall()
	 */
	public void uninstall() {
		// cancel possible running computation
		fMarkOccurrenceAnnotations= false;
		uninstallOccurrencesFinder();
		fPreferenceStore.removePropertyChangeListener(fPropertyChangeListener);

	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.ui.text.IStructuredTextOccurenceStructureProvider#initialize()
	 */
	public void initialize() {
		fPreferenceStore.addPropertyChangeListener(fPropertyChangeListener);

		fMarkOccurrenceAnnotations = fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey(
				XMLOccurrencePreferenceConstants.EDITOR_MARK_OCCURRENCES,
				fEditorID));

		fMarkNodeOccurrences = 
			fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey(
				XMLOccurrencePreferenceConstants.EDITOR_MARK_NODE_OCCURRENCES,
				fEditorID));

		
		fMarkAttributeOccurrences = 
			fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey(
				XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_OCCURRENCES,
				fEditorID));

		fMarkAttributeValueOccurrences = 
			fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey(
				XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES,
				fEditorID)); 

		fMarkTextOccurrences =
			fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey(
				XMLOccurrencePreferenceConstants.EDITOR_MARK_TEXT_OCCURRENCES,
				fEditorID));

		fStickyOccurrenceAnnotations = 
			fPreferenceStore.getBoolean(PreferenceKeyGenerator.generateKey( 
				XMLOccurrencePreferenceConstants.EDITOR_STICKY_OCCURRENCES,
				fEditorID));
	}
	
	ProjectionViewer getSourceViewer() {
		return fViewer;
	}

	protected void installOccurrencesFinder() {
		
		fPostSelectionListenerWithSM= new ISelectionListenerWithSM() {
			public void selectionChanged(IEditorPart part, ITextSelection selection, IStructuredModel model) {
				updateOccurrenceAnnotations(selection, model);
			}
		};
		SelectionListenerWithSMManager.getDefault().addListener(fEditor, fPostSelectionListenerWithSM);
		if (fEditor.getSelectionProvider() != null) {
			fForcedMarkOccurrencesSelection= fEditor.getSelectionProvider().getSelection();
			SelectionListenerWithSMManager.getDefault().forceSelectionChange(fEditor, (ITextSelection)fForcedMarkOccurrencesSelection);
		}
		
		if (fOccurrencesFinderJobCanceler == null) {
			fOccurrencesFinderJobCanceler= new OccurrencesFinderJobCanceler();
			fOccurrencesFinderJobCanceler.install();
		}
	}
	
	protected void uninstallOccurrencesFinder() {
		fMarkOccurrenceAnnotations= false;
		
		if (fOccurrencesFinderJob != null) {
			fOccurrencesFinderJob.cancel();
			fOccurrencesFinderJob= null;
		}

		if (fOccurrencesFinderJobCanceler != null) {
			fOccurrencesFinderJobCanceler.uninstall();
			fOccurrencesFinderJobCanceler= null;
		}
		
		if (fPostSelectionListenerWithSM != null) {
			SelectionListenerWithSMManager.getDefault().removeListener(fEditor, fPostSelectionListenerWithSM);
			fPostSelectionListenerWithSM= null;
		}
		
		removeOccurrenceAnnotations();
	}

	
	void removeOccurrenceAnnotations() {
		IDocumentProvider documentProvider= fEditor.getDocumentProvider();
		if (documentProvider == null)
			return;
		
		IAnnotationModel annotationModel= documentProvider.getAnnotationModel(fEditor.getEditorInput());
		if (annotationModel == null || fOccurrenceAnnotations == null)
			return;

		synchronized (getLockObject(annotationModel)) {
			if (annotationModel instanceof IAnnotationModelExtension) {
				((IAnnotationModelExtension)annotationModel).replaceAnnotations(fOccurrenceAnnotations, null);
			} else {
				for (int i= 0, length= fOccurrenceAnnotations.length; i < length; i++)
					annotationModel.removeAnnotation(fOccurrenceAnnotations[i]);
			}
			fOccurrenceAnnotations= null;
		}
	}

	/**
	 * Updates the occurrences annotations based
	 * on the current selection.
	 * 
	 * @param selection the text selection
	 * @param astRoot the compilation unit AST
	 * @since 3.0
	 */
	protected void updateOccurrenceAnnotations(ITextSelection selection, IStructuredModel model) {

		if (fOccurrencesFinderJob != null)
			fOccurrencesFinderJob.cancel();

		if (!fMarkOccurrenceAnnotations)
			return;
		
		if (model == null || selection == null)
			return;
		
		IDocument document= getSourceViewer().getDocument();
		if (document == null)
			return;
				
		String message;
		
		Document dom = (model instanceof IDOMModel) ? ((IDOMModel) model).getDocument() : null;

		List matches = findMatches(dom, model, selection);
		if (matches == null || matches.size() == 0) {
			if (!fStickyOccurrenceAnnotations)
				removeOccurrenceAnnotations();
			return;
		}
		
		List positionsList = new ArrayList();
		int i= 0;
		for (Iterator each= matches.iterator(); each.hasNext();) {
			Object current = each.next();
			if (current instanceof IDOMElement) {
				IDOMElement currentNode = (IDOMElement)current;
				
				// Add start tag
				int start = currentNode.getStartOffset();
				int end = currentNode.getStartEndOffset();
				positionsList.add(new Position(start, end - start));
				if (currentNode.hasEndTag()) {
					//Add end tag
					start = currentNode.getEndStartOffset();
					end = currentNode.getEndOffset();
					positionsList.add(new Position(start, end - start));
				}
				
			} else if (current instanceof ITextRegion) {
				ITextRegion currentRegion = (ITextRegion)current;
				positionsList.add(new Position(currentRegion.getStart(), currentRegion.getLength()));
			} else if (current instanceof Position) {
				Position currentPosition = (Position)current;
				positionsList.add(new Position(currentPosition.getOffset(), currentPosition.getLength()));
			}
		}
		if (positionsList.size() == 0) return;
		fOccurrencesFinderJob= new OccurrencesFinderJob(document, 
				(Position[])positionsList.toArray(new Position[positionsList.size()]), selection);
		fOccurrencesFinderJob.run(new NullProgressMonitor());
	}

	private List findMatches(Document dom, IStructuredModel model, ITextSelection selection) {
		try {
			IndexedRegion region = ((IDOMModel)model).getIndexedRegion(selection.getOffset());
			
			if (region instanceof IDOMElement) {
				IDOMElement xmlElement = (IDOMElement)region;
				NamedNodeMap attrs = xmlElement.getAttributes();
				for (int i = 0; attrs != null && i < attrs.getLength(); i++) {
					IDOMAttr xmlAttr = (IDOMAttr)attrs.item(i);
					if (xmlAttr.contains(selection.getOffset())) {
						if (selection.getOffset() >= xmlAttr.getNameRegionStartOffset() &&
							selection.getOffset() <= xmlAttr.getNameRegionEndOffset()) {
							return findTagAttrNameMatches(dom, xmlElement.getTagName(), xmlAttr.getName());
						} else if (selection.getOffset() >= xmlAttr.getValueRegionStartOffset() && 
								selection.getOffset() <= xmlAttr.getEndOffset()) {
							return findTagAttrValueMatches(dom, xmlAttr.getValue());
						}
					}
				}
				return findTagMatches(dom, xmlElement.getTagName());
			} else if (region instanceof IDOMText) {
				IDOMText xmlText = (IDOMText)region;
				return findTextMatches(dom, xmlText.getData().trim());
			}
		} catch (DOMException x) {
			XmlEditorPlugin.getPluginLog().logError(x);
		}
		return new ArrayList();
	}
	
	private List findTagMatches(Document dom, String tagName) {
		List matches = new ArrayList();
		
		if (fMarkNodeOccurrences) {
			NodeList children = (NodeContainer)dom.getChildNodes();
			for (int i = 0; children != null && i < children.getLength(); i++) {
				if(children.item(i) instanceof IDOMNode) {
					IDOMNode xmlNode = (IDOMNode)children.item(i);
					addTagOccurencies(matches, xmlNode, tagName);
				}
			}
		}
		return matches;
	}

	private void addTagOccurencies(List matches, IDOMNode xmlNode, String tagName) {
		if (matches == null || xmlNode == null || tagName == null) return;
		
			if (xmlNode instanceof IDOMElement) {
				IDOMElement xmlElement = (IDOMElement)xmlNode;
				if (tagName.equals(xmlElement.getTagName()))
					matches.add(xmlElement);
			}
			for (Node child = xmlNode.getFirstChild(); child != null; child = child.getNextSibling()) {
				if (child instanceof IDOMNode) {
					addTagOccurencies(matches, (IDOMNode)child, tagName);
				}
			}
	}	

	private List findTagAttrNameMatches(Document dom, String tagName, String attrName) {
		List matches = new ArrayList();
		
		if (fMarkAttributeOccurrences) {
			NodeList children = (NodeContainer)dom.getChildNodes();
			for (int i = 0; children != null && i < children.getLength(); i++) {
				if(children.item(i) instanceof IDOMNode) {
					IDOMNode xmlNode = (IDOMNode)children.item(i);
					addTagAttrNameOccurencies(matches, xmlNode, attrName);
				}
			}
		}
		return matches;
	}

	private void addTagAttrNameOccurencies(List matches, IDOMNode xmlNode, String attrName) {
		if (matches == null || xmlNode == null ||  attrName == null) return;
		
		if (xmlNode instanceof IDOMElement) {
			IDOMElement xmlElement = (IDOMElement)xmlNode;
			NamedNodeMap attrs = xmlElement.getAttributes();
			for (int i = 0; attrs != null && i < attrs.getLength(); i++) {
				IDOMAttr xmlAttr = (IDOMAttr)attrs.item(i);
				if (attrName.equals(xmlAttr.getName())) {
					matches.add(new Position(xmlAttr.getNameRegionStartOffset(), xmlAttr.getNameRegionTextEndOffset() - xmlAttr.getNameRegionStartOffset()));
				}
			}
		}
		for (Node child = xmlNode.getFirstChild(); child != null; child = child.getNextSibling()) {
			if (child instanceof IDOMNode) {
				addTagAttrNameOccurencies(matches, (IDOMNode)child, attrName);
			}
		}
	}	
	private List findTagAttrValueMatches(Document dom, String attrValue) {
		List matches = new ArrayList();

		if (fMarkAttributeValueOccurrences) {
			NodeList children = (NodeContainer)dom.getChildNodes();
			for (int i = 0; children != null && i < children.getLength(); i++) {
				if(children.item(i) instanceof IDOMNode) {
					IDOMNode xmlNode = (IDOMNode)children.item(i);
					addTagAttrValueOccurencies(matches, xmlNode, attrValue);
				}
			}
		}
		return matches;
	}

	private void addTagAttrValueOccurencies(List matches, IDOMNode xmlNode, String attrValue ) {
		if (matches == null || xmlNode == null || attrValue == null) return;
		
		if (xmlNode instanceof IDOMElement) {
			IDOMElement xmlElement = (IDOMElement)xmlNode;
			NamedNodeMap attrs = xmlElement.getAttributes();
			for (int i = 0; attrs != null && i < attrs.getLength(); i++) {
				IDOMAttr xmlAttr = (IDOMAttr)attrs.item(i);
				String value = xmlAttr.getValueRegionText();
				if (attrValue.equals(xmlAttr.getValue()))
					matches.add(new Position(xmlAttr.getValueRegionStartOffset(), value == null ? 0 : value.length()));
			}
		}
		for (Node child = xmlNode.getFirstChild(); child != null; child = child.getNextSibling()) {
			if (child instanceof IDOMNode) {
				addTagAttrValueOccurencies(matches, (IDOMNode)child, attrValue);
			}
		}
	}	

	private List findTextMatches(Document dom, String data) {
		List matches = new ArrayList();
		
		if (fMarkTextOccurrences && !isEmptyString (data)) { 
			NodeList children = (NodeContainer)dom.getChildNodes();
			for (int i = 0; children != null && i < children.getLength(); i++) {
				if (children.item(i) instanceof IDOMNode) {
					IDOMNode xmlNode = (IDOMNode)children.item(i);
					addTextOccurencies(matches, xmlNode, data);
				}
			}
		}
		return matches;
	}
	
	private boolean isEmptyString(String str) {
		return str == null || str.trim().length() == 0;
	}
	
	private void addTextOccurencies(List matches, IDOMNode xmlNode, String data) {
		if (matches == null || xmlNode == null || data == null) return;
		
			if (xmlNode instanceof IDOMText) {
				IDOMText xmlText = (IDOMText)xmlNode;
				String xmlData = xmlText.getData();
				if (xmlData != null && xmlData.indexOf(data) != -1) {
					matches.add(
						new Position(
							xmlText.getStartOffset() + xmlData.indexOf(data), data.length()));
				}
			}
			for (Node child = xmlNode.getFirstChild(); child != null; child = child.getNextSibling()) {
				if (child instanceof IDOMNode) {
					addTextOccurencies(matches, (IDOMNode)child, data);
				}
			}
	}	

	private boolean getEventNewBooleanValue(PropertyChangeEvent event) {
		return Boolean.getBoolean(event.getNewValue().toString());
	}

	/*
	 * @see AbstractTextEditor#handlePreferenceStoreChanged(PropertyChangeEvent)
	 */
	public void handlePreferenceStoreChanged(PropertyChangeEvent event) {
		
		ISourceViewer sourceViewer= getSourceViewer();
		if (getSourceViewer() != null) {
			String property= event.getProperty();	
			if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_MARK_OCCURRENCES,
					fEditorID).equals(property)) {
				boolean markOccurrences = getEventNewBooleanValue(event);
				if (markOccurrences != fMarkOccurrenceAnnotations) {
					fMarkOccurrenceAnnotations= markOccurrences;
	
					if (!fMarkOccurrenceAnnotations) {
						uninstallOccurrencesFinder();
					} else {
						installOccurrencesFinder();
					}
				}
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_OCCURRENCE_PROVIDER,
					fEditorID).equals(property)) {
				if (event.getNewValue() instanceof String) {
					String occurrenceProvider= (String)event.getNewValue();
					if (occurrenceProvider != fOccurrenceProvider) {
						fOccurrenceProvider= occurrenceProvider;
						if (fMarkOccurrenceAnnotations) {
							uninstallOccurrencesFinder();
							installOccurrencesFinder();
						}
					}
				}
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_MARK_NODE_OCCURRENCES,
					fEditorID).equals(property)) {
				fMarkNodeOccurrences= getEventNewBooleanValue(event);
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_OCCURRENCES,
					fEditorID).equals(property)) {
				fMarkAttributeOccurrences= getEventNewBooleanValue(event);
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_MARK_ATTRIBUTE_VALUE_OCCURRENCES,
					fEditorID).equals(property)) {
				fMarkAttributeValueOccurrences= getEventNewBooleanValue(event);
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_MARK_TEXT_OCCURRENCES,
					fEditorID).equals(property)) {
					fMarkAttributeValueOccurrences= getEventNewBooleanValue(event);
			} else if (PreferenceKeyGenerator.generateKey(
					XMLOccurrencePreferenceConstants.EDITOR_STICKY_OCCURRENCES,
					fEditorID).equals(property)) {
				fStickyOccurrenceAnnotations= getEventNewBooleanValue(event);
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.ibm.sse.editor.ui.text.IStructuredTextOccurenceStructureProvider#affectsPreferences(java.lang.String)
	 */
	public boolean affectsPreferences(String property) {
		return XMLOccurrencePreferenceConstants.affectsPreferences(property);
	}

	public IPreferenceStore getPreferenceStore() {
		return fPreferenceStore;
	}

	public String getEditorId() {
		return fEditorID;
	}

	
}
