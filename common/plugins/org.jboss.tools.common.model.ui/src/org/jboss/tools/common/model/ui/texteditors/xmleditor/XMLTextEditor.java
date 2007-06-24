/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.texteditors.xmleditor;

import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.dnd.ModelTransfer;
import org.jboss.tools.common.model.ui.texteditors.dnd.TextEditorDrop;
import org.jboss.tools.common.model.ui.texteditors.dnd.TextEditorDropProvider;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentListener;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextEvent;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.IVerticalRuler;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.DropTargetListener;
import org.eclipse.swt.dnd.FileTransfer;
import org.eclipse.swt.dnd.HTMLTransfer;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.ILocationProvider;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion;
import org.eclipse.wst.sse.ui.StructuredTextEditor;
import org.eclipse.wst.sse.ui.internal.StructuredTextViewer;
import org.eclipse.wst.xml.core.internal.document.AttrImpl;
import org.eclipse.wst.xml.core.internal.document.ElementImpl;
import org.eclipse.wst.xml.ui.internal.XMLUIPlugin;
import org.w3c.dom.DocumentType;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

import org.jboss.tools.common.meta.action.XActionInvoker;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.text.xml.ui.FreeCaretStyledText;
import org.jboss.tools.common.model.ui.editor.EditorDescriptor;
import org.jboss.tools.common.model.ui.editor.IModelObjectEditorInput;
import org.jboss.tools.common.model.ui.editors.dnd.DropCommandFactory;
import org.jboss.tools.common.model.ui.editors.dnd.DropData;
import org.jboss.tools.common.model.ui.editors.dnd.EmptyTagProposalFactory;
import org.jboss.tools.common.model.ui.editors.dnd.context.DropContext;
import org.jboss.tools.common.model.ui.editors.dnd.context.IDNDTextEditor;
import org.jboss.tools.common.model.ui.editors.dnd.context.InnerDragBuffer;
import org.jboss.tools.common.model.ui.views.palette.PaletteInsertHelper;
import org.jboss.tools.common.text.xml.IOccurrencePreferenceProvider;
import org.jboss.tools.common.text.xml.RedHatStructuredTextViewerConfigurationXML;
import org.jboss.tools.common.text.xml.XmlEditorPlugin;
import org.jboss.tools.jst.jsp.text.xpl.IStructuredTextOccurrenceStructureProvider;

/**
 * @author Jeremy
 *
 */
public class XMLTextEditor extends StructuredTextEditor implements IDocumentListener, IDNDTextEditor, IOccurrencePreferenceProvider {
	private IStructuredTextOccurrenceStructureProvider fOccurrenceModelUpdater;
///	TextEditorDrop dnd = new TextEditorDrop();
	IEditorInput input;
	XModelObject object = null;

	public XMLTextEditor() {
		XmlEditorPlugin.getDefault().initDefaultPluginPreferences();
///		dnd.setTextEditorDropProvider(new TextEditorDropProviderImpl());
		super.setSourceViewerConfiguration(new RedHatStructuredTextViewerConfigurationXML());
	}

	protected void setSourceViewerConfiguration(SourceViewerConfiguration config) {
		if(!(config instanceof RedHatStructuredTextViewerConfigurationXML)) {
			RedHatStructuredTextViewerConfigurationXML r = new RedHatStructuredTextViewerConfigurationXML();
			r.setInitialConfiguration(config);
			config = r;
		}
		super.setSourceViewerConfiguration(config);
	}

	protected void initializeDrop(ITextViewer textViewer) {
		//fake drop to make fDropTarget != null.
		Composite c = textViewer.getTextWidget();
		Label l = new Label(c, SWT.NONE);
// TODO migration to new WTP is needed
//		fDropTarget = new DropTarget(l, 0);
		l.dispose();
	}

	public IStructuredTextOccurrenceStructureProvider getOccurrencePreferenceProvider() {
		return fOccurrenceModelUpdater;
	}

	public String getEditorId() {
		return XMLUIPlugin.ID;
	}

	public void createPartControl(Composite parent) {
		super.createPartControl(parent);

		fOccurrenceModelUpdater= XmlEditorPlugin.getDefault().getOccurrenceStructureProviderRegistry(XmlEditorPlugin.PLUGIN_ID).getCurrentOccurrenceProvider(XmlEditorPlugin.PLUGIN_ID);
		if (fOccurrenceModelUpdater != null)
			fOccurrenceModelUpdater.install(this, getTextViewer());

///		dnd.enable();
		createDrop();
		setModified(false);
		getDocumentListenerRegister().unregister();
		getDocumentListenerRegister().register();
        try {
            Object dtid =
                getSourceViewer().getTextWidget().getData("DropTarget");
            if (dtid != null) {
                if (dtid instanceof DropTarget) {
                    DropTarget dropTarget = (DropTarget) dtid;
                    dropTarget.addDropListener(new DropTargetAdapter() {
                        private FreeCaretStyledText getFreeCaretControl(Object sourceOrTarget) {
                            if (sourceOrTarget == null)
                                return null;

                            Object control = null;

                            if (sourceOrTarget instanceof DropTarget) {
                                control =
                                    ((DropTarget) sourceOrTarget).getControl();
                            } else if (sourceOrTarget instanceof DragSource) {
                                control =
                                    ((DragSource) sourceOrTarget).getControl();
                            } else
                                return null;

                            if (control instanceof FreeCaretStyledText)
                                return (FreeCaretStyledText) control;
                            return null;
                        }

                        private int getPosition(StyledText t, int x, int y) {
                            try {
                                Point pp = t.toControl(x, y);
                                x = pp.x;
                                y = pp.y;
                                int lineIndex =
                                    (t.getTopPixel() + y) / t.getLineHeight();
                                if (lineIndex >= t.getLineCount()) {
                                    return t.getCharCount();
                                } else {
                                    int c = 0;
                                    try {
                                        c =
                                            t.getOffsetAtLocation(
                                                new Point(x, y));
                                        if (c < 0)
                                            c = 0;
                                    } catch (Exception ex) {
                                        c =
                                            t.getOffsetAtLine(lineIndex + 1)
                                                - (t.getLineDelimiter() == null
                                                    ? 0
                                                    : t
                                                        .getLineDelimiter()
                                                        .length());
                                    }
                                    return c;
                                }
                            } catch (Exception e) {
                                return 0;
                            }
                        }

                        public void dragEnter(DropTargetEvent event) {
                            try {
                                getFreeCaretControl(
                                    event.widget).enableFreeCaret(
                                    true);
                            } catch (Exception x) {
                            	//ignore
                            }
                        }

                        public void dragLeave(DropTargetEvent event) {
                            try {
                                getFreeCaretControl(
                                    event.widget).enableFreeCaret(
                                    false);
                            } catch (Exception x) {
                            	//ignore
                            }
                        }

                        public void dragOperationChanged(DropTargetEvent event) {
                            try {
                                getFreeCaretControl(
                                    event.widget).enableFreeCaret(
                                    false);
                            } catch (Exception x) {
                            	//ignore
                            }
                        }

                        public void dragOver(DropTargetEvent event) {
                            try {
                                FreeCaretStyledText fcst =
                                    getFreeCaretControl(event.widget);
                                int pos = getPosition(fcst, event.x, event.y);
                                Point p = fcst.getLocationAtOffset(pos);
                                fcst.myRedraw(p.x, p.y);
                            } catch (Exception x) {
                            	//ignore
                            }
                        }

                        public void drop(DropTargetEvent event) {
                            try {
                                getFreeCaretControl(
                                    event.widget).enableFreeCaret(
                                    false);
                            } catch (Exception x) {
                            	//ignore
                            }
                        }
                    });
                }

            }
        } catch (Exception x) {
        	//ignore
        }

	}

	/*
	 * @see org.eclipse.ui.texteditor.AbstractTextEditor#createSourceViewer(Composite, IVerticalRuler, int)
	 * @since 2.1
	 */
	protected ISourceViewer createSourceViewer(Composite parent, IVerticalRuler ruler, int styles) {
		ISourceViewer sv = super.createSourceViewer(parent, ruler, styles);
		
		sv.getTextWidget().addFocusListener(new TextFocusListener());
//		sv.addTextListener(this);
		
		return sv;		
	}
	
	protected StructuredTextViewer createStructedTextViewer(Composite parent, IVerticalRuler verticalRuler, int styles) {
		return new StructuredTextViewer(parent, verticalRuler, getOverviewRuler(), isOverviewRulerVisible(), styles) {
		    /**
		     * Factory method to create the text widget to be used as the viewer's text widget.
		     * 
		     * @return the text widget to be used
		     */
		    protected StyledText createTextWidget(Composite parent, int styles) {
		        return new FreeCaretStyledText(parent, styles);
		    }
		};
	}

	class TextFocusListener extends FocusAdapter {
		public void focusLost(FocusEvent e) {
			if(!XMLTextEditor.super.isDirty()) return;
			Display.getDefault().syncExec( 
				new Runnable() {
					public void run() {
						try { Thread.sleep(200); } catch (Exception e) {}
						save();
					}
				}
			);			
		}
	}

	public void save() {
		if(!lock && isModified()) {		
			lock = true;
			try {
				FileAnyImpl f = (FileAnyImpl)getModelObject();
				if(f != null) f.edit(getSourceViewer().getDocument().get());						
			} catch (Exception e) {
				ModelUIPlugin.log(e);
			} finally {
				setModified(false);
				lock = false;
			}
		}
		getSourceViewer().getAnnotationModel().disconnect(getSourceViewer().getDocument());
		getSourceViewer().getAnnotationModel().connect(getSourceViewer().getDocument());
	}

	boolean modified = false;

	public void setModified(boolean set) {
		if (this.modified != set) {
			this.modified = set;
			if(set) {
				XModelObject o = getModelObject();
				if(o != null) o.setModified(true);
			}
			super.firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}
	
	public boolean isModified() {
		return this.modified;
	}
	
	protected void doSetInput(IEditorInput input) throws CoreException {
		super.doSetInput(input);
		this.input = input;
		if (input instanceof IModelObjectEditorInput){
			try {
				object = ((IModelObjectEditorInput)input).getXModelObject();
			} catch (Exception ex) {
				ModelUIPlugin.log("Error while getting model object from editor input", ex);
			}
		}
		if(getSourceViewer() != null && getSourceViewer().getDocument() != null) {
			getDocumentListenerRegister().unregister();
			getDocumentListenerRegister().register();
		}
		
	}

	/*
	 * @see org.eclipse.ui.editors.text.TextEditor#getAdapter(Class)
	 */
	public Object getAdapter( Class adapter ) {
		if (adapter == EditorDescriptor.class)
			return new EditorDescriptor("xml");
		return super.getAdapter( adapter );
	}

	boolean lock = false;
	public boolean isDirty() {
		if (getEditorInput() instanceof IModelObjectEditorInput) {
			XModelObject o = getModelObject();
			if(o != null && o.isModified())
				return true;
			else {
				return isModified();
			} 
		} else {
			return super.isDirty();
		}
	}

	public boolean isSaveOnCloseNeeded() {
		return super.isSaveOnCloseNeeded() || isModified();
	}

	public void doSave(IProgressMonitor monitor) {
//		ModelUIPlugin.log(getSourceViewer().getDocument().get());
		XModelObject o = getModelObject();
		if(o != null) {
			save();
			if(!(getEditorInput() instanceof IFileEditorInput) && 
					(getEditorInput() instanceof ILocationProvider)) {
				doSaveYourself();
			} else {
				o.setModified(false);
				XModelObjectLoaderUtil.updateModifiedOnSave(o);
			}
		}
		superDoSave(monitor);
	}
	
	protected final void superDoSave(IProgressMonitor monitor) {
		super.doSave(monitor);
	}
	
	protected void doSaveYourself() {}

	public XModelObject getModelObject() {
		if (getEditorInput() instanceof IModelObjectEditorInput) {
			return ((IModelObjectEditorInput)getEditorInput()).getXModelObject();
		}
		return null;
	}
	
	class TextEditorDropProviderImpl implements TextEditorDropProvider, TextEditorDrop.TextEditorDropProvider2 {

		public ISourceViewer getSourceViewer() {
			return XMLTextEditor.this.getSourceViewer();
		}

		public XModelObject getModelObject() {
			return XMLTextEditor.this.getModelObject();
		}
	
		public void insert(Properties p) {
			PaletteInsertHelper.insertIntoEditor(getSourceViewer(), p);
		}

		public String getContext(int pos) {
			return _getContext(pos);
		}

	}
	
	int lastpos = -1;
	String lastContext = null;
	
	private String _getContext(int pos) {
        if(lastpos == pos && pos >= 0) {
        	pos = lastpos;
        	return lastContext;
        }
        lastpos = pos;
        lastContext = null;
        getSourceViewer().getDocument();
        IndexedRegion region = getModel().getIndexedRegion(pos);
        if(region instanceof ElementImpl) {
        	ElementImpl jspElement = (ElementImpl)region;
        	NamedNodeMap attributes = jspElement.getAttributes();
        	if(pos==jspElement.getStartOffset()
        			|| pos==jspElement.getEndStartOffset()) {
    			return lastContext = "text";
        	}
        	for(int i = 0;i<attributes.getLength();i++ ) {
        		Node attribute = attributes.item(i);
        		if(attribute instanceof AttrImpl) {
        			AttrImpl jspAttr = (AttrImpl)attribute;
        			ITextRegion valueRegion = jspAttr.getValueRegion();
        			if(valueRegion == null) {
        				return lastContext = "none";
        			}
        			int startPos = jspElement.getStartOffset()+ valueRegion.getStart();
        			int endPos = jspElement.getStartOffset() + valueRegion.getTextEnd();
        			if(pos > startPos && pos < endPos) {
        				return lastContext = "attribute";
        			}
        		}
        	}
			return lastContext = "none";
        } else if(region instanceof Text) {
			return lastContext = "text";
        } else if(region instanceof DocumentType) {
			return lastContext = "none";
        } else if(region == null) {
        	//new place
			return lastContext = "text";
        }
		return lastContext = "none";
	}
	
	public void textChanged(TextEvent event) {
		setModified(super.isSaveOnCloseNeeded());
	}

	public void documentAboutToBeChanged(DocumentEvent event) {}

	public void documentChanged(DocumentEvent event) {
		textChanged(null);
	}

	public void doRevertToSaved() {
		save();
		XModelObject o = getModelObject();
		Properties p = new Properties();
		XActionInvoker.invoke("FileTXT", "DiscardActions.Discard", o, p);
		if(!"true".equals(p.getProperty("done"))) return;
		super.doRevertToSaved();
		if(o.isModified()) o.setModified(false);
		modified = false;
		firePropertyChange(IEditorPart.PROP_DIRTY);
		updatePartControl(getEditorInput());
	}
	
	public void dispose() {
		getDocumentListenerRegister().unregister();
		documentListenerRegistry = null;
		if (fOccurrenceModelUpdater != null) {
			fOccurrenceModelUpdater.uninstall();
			fOccurrenceModelUpdater = null;
		}
		super.dispose();
	}
	
	//////dnd
	
	private void createDrop() {
		DropTarget target = new DropTarget(getSourceViewer().getTextWidget(), DND.DROP_MOVE | DND.DROP_COPY);
		Transfer[] types = new Transfer[] {ModelTransfer.getInstance(), HTMLTransfer.getInstance(), TextTransfer.getInstance(), FileTransfer.getInstance()};
		target.setTransfer(types);
		target.addDropListener(new DTL());		
	}
	
	class DTL implements DropTargetListener {
		DropContext dropContext = new DropContext();
		int lastpos = -1;
		int lastdetail = -1;
		
		public void dragEnter(DropTargetEvent event) {
			lastpos = -1;
		}
		public void dragLeave(DropTargetEvent event) {
			lastpos = -1;
		}
		public void dragOperationChanged(DropTargetEvent event) {
		}

		public void dragOver(DropTargetEvent event) {
			if(!isEditable() || (getModelObject() != null && !getModelObject().isObjectEditable())) {
    			event.detail = DND.DROP_NONE;
				return;
			}
			dropContext.setDropTargetEvent(event);
			if(dropContext.getFlavor() == null)  {
    			event.detail = DND.DROP_NONE;
				return;
			}
			// Drop from VPE to Source is forbidden
			if(dropContext.getFlavor().equals("text/html")) {
				if(InnerDragBuffer.object != null) {
					event.detail = DND.DROP_NONE;
				}
    			return;
			}
            int pos = getPosition(event.x, event.y);
            if(lastpos == pos && pos >= 0) {
            	pos = lastpos;
            	event.detail = lastdetail;
            	return;
            }
            lastpos = pos;
			dropContext.clean();
            getSourceViewer().getDocument();
            IndexedRegion region = getModel().getIndexedRegion(pos);
            if(region instanceof ElementImpl) {
            	ElementImpl jspElement = (ElementImpl)region;
            	NamedNodeMap attributes = jspElement.getAttributes();
            	if(pos==jspElement.getStartOffset()
            			|| pos==jspElement.getEndStartOffset()) {
        			event.detail = lastdetail = DND.DROP_MOVE;
        			return;
            	}
            	for(int i = 0;i<attributes.getLength();i++ ) {
            		Node attribute = attributes.item(i);
            		if(attribute instanceof AttrImpl) {
            			AttrImpl jspAttr = (AttrImpl)attribute;
            			ITextRegion valueRegion = jspAttr.getValueRegion();
            			if(valueRegion == null) {
            				event.detail = lastdetail = DND.DROP_NONE;
            				return;
            			}
            			int startPos = jspElement.getStartOffset()+ valueRegion.getStart();
            			int endPos = jspElement.getStartOffset() + valueRegion.getTextEnd();
            			if(pos>startPos && pos<endPos) {
            				dropContext.setOverAttributeValue(true);
            				event.detail = lastdetail = DND.DROP_MOVE;
            				return;
            			}
            		}
            	}
    			event.detail = lastdetail = DND.DROP_NONE;            	
            } else if(region instanceof Text) {
    			event.detail = lastdetail = DND.DROP_MOVE;            	
            } else if(region instanceof DocumentType) {
    			event.detail = lastdetail = DND.DROP_NONE;            	
            } else if(region == null) {
            	//new place
    			event.detail = lastdetail = DND.DROP_MOVE;            	
            }
		}	

		public void drop(DropTargetEvent event) {
			int offset = getPosition(event.x, event.y);
			selectAndReveal(offset, 0);
			dropContext.runDropCommand(XMLTextEditor.this, event);
		}

		public void dropAccept(DropTargetEvent event) {
		}
		
	}
	public void runDropCommand(final String flavor, final String data) {
		Display.getDefault().asyncExec(new Runnable() {
			public void run() {
				DropCommandFactory.getInstance()
				.getDropCommand(flavor, EmptyTagProposalFactory.getInstance())
				.execute(
					new DropData(
						flavor,
						data,
						null, ///getPageContext(),
						getEditorInput(),
						getSourceViewer(),
						getSelectionProvider()
					)
				);		
			}
		});
	}
	
	private int getPosition(int x, int y) {
		try {
			ISourceViewer v = getSourceViewer();
			StyledText t = v.getTextWidget();
			Point pp = t.toControl(x, y);
			x = pp.x;
			y = pp.y;		
			int lineIndex = (t.getTopPixel() + y) / t.getLineHeight();
			if (lineIndex >= t.getLineCount()) {
				return t.getCharCount();
			} else {
				int c = 0;
				try {
					c = t.getOffsetAtLocation(new Point(x, y));
					if(c < 0) c = 0;
				} catch (Exception ex) {
                    c = t.getOffsetAtLine(lineIndex + 1) - 
                    (t.getLineDelimiter() == null ? 0 : t.getLineDelimiter().length());					
				}
				return c;
			}
		} catch (Exception e) {
			return 0;
		}
	}
	
	private DocumentListenerRegistry documentListenerRegistry;
	
	protected DocumentListenerRegistry getDocumentListenerRegister() {
		if(documentListenerRegistry == null) {
			documentListenerRegistry = new DocumentListenerRegistry();
		}
		return documentListenerRegistry;
	}
	
	protected class DocumentListenerRegistry {
		IDocument document = null;
		
		public void unregister() {
			if(document != null) {
				document.removeDocumentListener(XMLTextEditor.this);
			}
		}
		
		public void register() {
			if(getSourceViewer() == null) return;
			document = getSourceViewer().getDocument();
			if(document != null) {
				document.addDocumentListener(XMLTextEditor.this);
			}
		}
	}

}
