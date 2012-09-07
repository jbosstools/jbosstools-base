/*******************************************************************************
 * Copyright (c) 2000, 2011 IBM Corporation and others.
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
package org.jboss.tools.common.text.xml.xpl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.jface.text.AbstractInformationControl;
import org.eclipse.jface.text.AbstractReusableInformationControlCreator;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IInformationControlExtension2;
import org.eclipse.jface.text.IInformationControlExtension4;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.IRewriteTarget;
import org.eclipse.jface.text.ITextHoverExtension;
import org.eclipse.jface.text.ITextHoverExtension2;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITextViewerExtension;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.SourceViewer;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseListener;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.ScrollBar;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.editors.text.EditorsUI;
import org.eclipse.ui.texteditor.DefaultMarkerAnnotationAccess;
import org.eclipse.ui.texteditor.SimpleMarkerAnnotation;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.eclipse.wst.sse.ui.internal.taginfo.ProblemAnnotationHoverProcessor;
import org.jboss.tools.common.quickfix.MarkerAnnotationInfo;
import org.jboss.tools.common.quickfix.MarkerAnnotationInfo.AnnotationInfo;
import org.jboss.tools.common.text.xml.TextXMLMessages;

public class MarkerProblemAnnotationHoverProcessor extends ProblemAnnotationHoverProcessor implements ITextHoverExtension, ITextHoverExtension2{
	
	private IInformationControlCreator controlCreator = null;
	private static IInformationControlCreator presenterControlCreator;

	public String getHoverInfo(ITextViewer viewer, IRegion hoverRegion) {
		return null;
	}

	public Object getHoverInfo2(ITextViewer viewer, IRegion hoverRegion) {
		List<AnnotationInfo> all = new ArrayList<AnnotationInfo>();
		List<AnnotationInfo> high = new ArrayList<AnnotationInfo>();
		List<AnnotationInfo> low = new ArrayList<AnnotationInfo>();
		List<Annotation> annotations = new ArrayList<Annotation>();

		IAnnotationModel model = ((SourceViewer) viewer).getAnnotationModel();
		if (model != null) {
			Iterator<Annotation> iterator = model.getAnnotationIterator();
			while (iterator.hasNext()) {
				Annotation annotation = (Annotation) iterator.next();
				if (!isAnnotationValid(annotation))
					continue;
				if(annotations.contains(annotation))
					continue;
				annotations.add(annotation);

				Position position = model.getPosition(annotation);

				if (position.overlapsWith(hoverRegion.getOffset(), hoverRegion.getLength())) {
					AnnotationInfo info = new AnnotationInfo(annotation, position);
					if(info.isTop()){
						high.add(info);
					}else{
						low.add(info);
					}
				}
			}
			all.addAll(high);
			all.addAll(low);
			
		}
		if(all.size() > 0)
			return new MarkerAnnotationInfo(all, (SourceViewer) viewer);
		return null;
	}
	
	public IInformationControlCreator getInformationPresenterControlCreator() {
		if (presenterControlCreator == null)
			presenterControlCreator= new PresenterControlCreator();
		return presenterControlCreator;
	}
	
	protected boolean isAnnotationValid(Annotation annotation) {
		if(annotation instanceof SimpleMarkerAnnotation || annotation instanceof TemporaryAnnotation)
			return true;
		return false;
	}

	public IInformationControlCreator getHoverControlCreator() {
		if (controlCreator == null)
			controlCreator= new AnnotationHoverControlCreator(getInformationPresenterControlCreator());
		return controlCreator;
	}
	
	private static class MarkerAnnotationInformationControl extends AbstractInformationControl implements IInformationControlExtension2{
		private final DefaultMarkerAnnotationAccess annotationAccess;
		private MarkerAnnotationInfo info;
		private Composite parent;

		public MarkerAnnotationInformationControl(Shell parentShell,
				String str) {
			super(parentShell, str);
			annotationAccess= new DefaultMarkerAnnotationAccess();
			create();
		}
		
		public boolean hasContents() {
			return info != null;
		}
		
		private MarkerAnnotationInfo getAnnotationInfo() {
			return info;
		}

		@Override
		protected void createContent(Composite parent) {
			this.parent = parent;
			GridLayout layout= new GridLayout(1, false);
			layout.verticalSpacing= 0;
			layout.marginWidth= 0;
			layout.marginHeight= 0;
			parent.setLayout(layout);
		}
		
		public final void setVisible(boolean visible) {
			if (!visible)
				disposeContent();
			super.setVisible(visible);
		}

		public Point computeSizeHint() {
			Point preferedSize= getShell().computeSize(SWT.DEFAULT, SWT.DEFAULT, true);

			Point constrains= getSizeConstraints();
			if (constrains == null)
				return preferedSize;

			Point constrainedSize= getShell().computeSize(constrains.x, SWT.DEFAULT, true);

			int width= Math.min(preferedSize.x, constrainedSize.x);
			int height= Math.max(preferedSize.y, constrainedSize.y);

			return new Point(width, height);
		}
		
		public void setInput(Object input) {
			Assert.isLegal(input instanceof MarkerAnnotationInfo);
			info = (MarkerAnnotationInfo)input;
			disposeContent();
			createContent();
		}
		
		protected void disposeContent() {
			for (Control child : parent.getChildren()) {
				child.dispose();
			}
		}
		
		protected void createContent() {
			boolean first = true;

			final ScrolledComposite scrolledComposite= new ScrolledComposite(parent, SWT.V_SCROLL | SWT.H_SCROLL);
			GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
			scrolledComposite.setLayoutData(gridData);
			scrolledComposite.setExpandVertical(false);
			scrolledComposite.setExpandHorizontal(false);
			
			Composite composite = new Composite(scrolledComposite, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
			GridLayout layout = new GridLayout(1, false);
			layout.marginLeft = 5;
			layout.verticalSpacing = 2;
			composite.setLayout(layout);

			
			for(AnnotationInfo info : getAnnotationInfo().infos){
				createInfo(composite, info.annotation, first);
				if(first)
					first = false;
	
				List<ICompletionProposal> proposals = getAnnotationInfo().getCompletionProposals(info);
				if (proposals.size() > 0)
					createControl(composite, proposals);
			}
			
			scrolledComposite.setContent(composite);
			setDecoration(scrolledComposite, parent.getForeground(), parent.getBackground(), JFaceResources.getDialogFont());

			Point contentSize= composite.computeSize(SWT.DEFAULT, SWT.DEFAULT);
			composite.setSize(contentSize);

			Point constraints= getSizeConstraints();
			if (constraints != null && contentSize.x < constraints.x) {
				ScrollBar horizontalBar= scrolledComposite.getHorizontalBar();

				int scrollBarHeight;
				if (horizontalBar == null) {
					Point scrollSize = scrolledComposite.computeSize(SWT.DEFAULT, SWT.DEFAULT);
					scrollBarHeight = scrollSize.y - contentSize.y;
				} else {
					scrollBarHeight = horizontalBar.getSize().y;
				}
				gridData.heightHint = contentSize.y - scrollBarHeight;
			}

			parent.layout(true);
		}
		
		private void createControl(Composite parent, List<ICompletionProposal> proposals) {
			Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
			GridLayout layout2 = new GridLayout(1, false);
			layout2.verticalSpacing = 2;
			layout2.marginLeft = 5;
			composite.setLayout(layout2);

			Label separator= new Label(composite, SWT.SEPARATOR | SWT.HORIZONTAL);
			GridData gridData= new GridData(SWT.FILL, SWT.CENTER, true, false);
			separator.setLayoutData(gridData);

			Label quickFixLabel= new Label(composite, SWT.NONE);
			GridData layoutData= new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
			layoutData.horizontalIndent = 4;
			quickFixLabel.setLayoutData(layoutData);
			String text;
			if (proposals.size() == 1) {
				text= TextXMLMessages.SINGLE_QUICK_FIX;
			} else {
				text= NLS.bind(TextXMLMessages.MULTIPLE_QUICK_FIX, proposals.size());
			}
			quickFixLabel.setText(text);

			createList(composite, proposals);
		}
		
		private void createList(Composite parent, List<ICompletionProposal> proposals) {
			List<Link> list = new ArrayList<Link>();
			for (ICompletionProposal proposal : proposals) {
				list.add(createLink(parent, proposal));
			}
		}

		private Link createLink(Composite parent, final ICompletionProposal proposal) {
			parent= new Composite(parent, SWT.NONE);
			GridLayout layout= new GridLayout(2, false);
			layout.marginWidth= 0;
			layout.marginHeight= 1;
			parent.setLayout(layout);
			
			Label proposalImage= new Label(parent, SWT.NONE);
			proposalImage.setLayoutData(new GridData(SWT.BEGINNING, SWT.CENTER, false, false));
			Image image= proposal.getImage();
			if (image != null) {
				proposalImage.setImage(image);

				proposalImage.addMouseListener(new MouseListener() {

					public void mouseDoubleClick(MouseEvent e) {
					}

					public void mouseDown(MouseEvent e) {
						fix(proposal, info.viewer, info.infos.get(0).position.getOffset());
					}

					public void mouseUp(MouseEvent e) {
					}

				});
			}
			Link proposalLink = new Link(parent, SWT.WRAP);
			GridData layoutData = new GridData(SWT.BEGINNING, SWT.CENTER, false, false);
			String linkText = proposal.getDisplayString();
			proposalLink.setText("<a>" + linkText + "</a>"); //$NON-NLS-1$ //$NON-NLS-2$
			proposalLink.setLayoutData(layoutData);
			proposalLink.addMouseListener(new MouseListener(){
				public void mouseDoubleClick(MouseEvent e) {
				}

				public void mouseDown(MouseEvent e) {
					fix(proposal, info.viewer, info.infos.get(0).position.getOffset());
				}

				public void mouseUp(MouseEvent e) {
				}
			});
			return proposalLink;
		}

		private void fix(ICompletionProposal p, ITextViewer viewer, int offset) {
			dispose();

			IRewriteTarget target = null;
			try {
				IDocument document = viewer.getDocument();

				if (viewer instanceof ITextViewerExtension) {
					ITextViewerExtension extension= (ITextViewerExtension) viewer;
					target= extension.getRewriteTarget();
				}

				if (target != null)
					target.beginCompoundChange();

				p.apply(document);

				Point selection = p.getSelection(document);
				if (selection != null) {
					viewer.setSelectedRange(selection.x, selection.y);
					viewer.revealRange(selection.x, selection.y);
				}
			} finally {
				if (target != null)
					target.endCompoundChange();
			}
		}
		
		private void createInfo(Composite parent, final Annotation annotation, boolean firstElement) {
			if(!firstElement){
				Label separator= new Label(parent, SWT.SEPARATOR | SWT.HORIZONTAL);
				GridData gridData= new GridData(SWT.FILL, SWT.CENTER, true, false);
				separator.setLayoutData(gridData);
			}
			
			Composite composite = new Composite(parent, SWT.NONE);
			composite.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));
			GridLayout layout = new GridLayout(2, false);
			layout.marginHeight = 2;
			layout.marginWidth = 2;
			layout.horizontalSpacing = 0;
			composite.setLayout(layout);

			final Canvas canvas = new Canvas(composite, SWT.NO_FOCUS);
			GridData gridData = new GridData(SWT.BEGINNING, SWT.BEGINNING, false, false);
			gridData.widthHint = 17;
			gridData.heightHint = 16;
			canvas.setLayoutData(gridData);
			canvas.addPaintListener(new PaintListener() {
				public void paintControl(PaintEvent e) {
					e.gc.setFont(null);
					annotationAccess.paint(annotation, e.gc, canvas, new Rectangle(0, 0, 16, 16));
				}
			});

			StyledText text = new StyledText(composite, SWT.MULTI | SWT.WRAP | SWT.READ_ONLY);
			GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
			text.setLayoutData(data);
			String annotationText = annotation.getText();
			if (annotationText != null)
				text.setText(annotationText);
		}
		
		private void setDecoration(Control control, Color foreground, Color background, Font font) {
			control.setForeground(foreground);
			control.setBackground(background);
			control.setFont(font);

			if (control instanceof Composite) {
				for (Control child : ((Composite) control).getChildren()) {
					setDecoration(child, foreground, background, font);
				}
			}
		}

	}
	
	private static final class AnnotationHoverControlCreator extends AbstractReusableInformationControlCreator {
		private final IInformationControlCreator presenterControlCreator;

		public AnnotationHoverControlCreator(IInformationControlCreator presenterControlCreator) {
			this.presenterControlCreator = presenterControlCreator;
		}

		public IInformationControl doCreateInformationControl(Shell parent) {
			return new MarkerAnnotationInformationControl(parent, EditorsUI.getTooltipAffordanceString()) {
				public IInformationControlCreator getInformationPresenterControlCreator() {
					return presenterControlCreator;
				}
			};
		}

		public boolean canReuse(IInformationControl control) {
			if (!super.canReuse(control))
				return false;

			if (control instanceof IInformationControlExtension4)
				((IInformationControlExtension4) control).setStatusText(EditorsUI.getTooltipAffordanceString());

			return true;
		}
	}
	
	private static final class PresenterControlCreator extends AbstractReusableInformationControlCreator {
		public IInformationControl doCreateInformationControl(Shell parent) {
			return new MarkerAnnotationInformationControl(parent, EditorsUI.getTooltipAffordanceString()) {
				public IInformationControlCreator getInformationPresenterControlCreator() {
					return presenterControlCreator;
				}
			};
		}
	}

}
