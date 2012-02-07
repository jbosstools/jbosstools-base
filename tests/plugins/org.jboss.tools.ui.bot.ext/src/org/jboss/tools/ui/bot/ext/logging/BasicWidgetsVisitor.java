package org.jboss.tools.ui.bot.ext.logging;

import javax.swing.table.TableColumn;

import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.ExpandBar;
import org.eclipse.swt.widgets.ExpandItem;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Link;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Slider;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tray;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.forms.widgets.Hyperlink;

/**
 * Distinguishes basic types that are known for SWTBot.  
 * 
 * @author Lucia Jelinkova
 *
 */
public abstract class BasicWidgetsVisitor implements WidgetVisitor {

	@Override
	public void visit(Widget widget) {
		if (widget instanceof Button){
			visitButton((Button) widget);
		} else if (widget instanceof Browser){
			visitBrowser((Browser) widget);
		} else if (widget instanceof CCombo){
			visitCCombo((CCombo) widget);
		} else if (widget instanceof CLabel){
			visitCLabel((CLabel) widget);
		} else if (widget instanceof Combo){
			visitCombo((Combo) widget);
		} else if (widget instanceof CTabItem){
			visitCTabItem((CTabItem) widget);
		} else if (widget instanceof DateTime){
			visitDateTime((DateTime) widget);
		} else if (widget instanceof ExpandBar){
			visitExpandBar((ExpandBar) widget);
		} else if (widget instanceof ExpandItem){
			visitExpandItem((ExpandItem) widget);
		} else if (widget instanceof Label){
			visitLabel((Label) widget);
		} else if (widget instanceof Link){
			visitLink((Link) widget);
		} else if (widget instanceof List){
			visitList((List) widget);
		} else if (widget instanceof Menu){
			visitMenu((Menu) widget);
		} else if (widget instanceof Scale){
			visitScale((Scale) widget);
		} else if (widget instanceof Shell){
			visitShell((Shell) widget);
		} else if (widget instanceof Slider){
			visitSlider((Slider) widget);
		} else if (widget instanceof Spinner){
			visitSpinner((Spinner) widget);
		} else if (widget instanceof StyledText){
			visitStyledText((StyledText) widget);
		} else if (widget instanceof TabItem){
			visitTabItem((TabItem) widget);
		} else if (widget instanceof Table){
			visitTable((Table) widget);
		} else if (widget instanceof TableItem){
			visitTableItem((TableItem) widget);
		} else if (widget instanceof Text){
			visitText((Text) widget);
		} else if (widget instanceof ToolBar){
			visitToolBar((ToolBar) widget);
		} else if (widget instanceof ToolItem){
			visitToolItem((ToolItem) widget);
		} else if (widget instanceof Tray){
			visitTray((Tray) widget);
		} else if (widget instanceof Tree){
			visitTree((Tree) widget);
		} else if (widget instanceof TreeItem){
			visitTreeItem((TreeItem) widget);
		} else if (widget instanceof Hyperlink){
			visitHyperLink((Hyperlink) widget);
		} else if (widget instanceof Group){
			visitGroup((Group) widget);
		} else {
			visitUnkownItem(widget);
		}
	}

	protected abstract void visitButton(Button widget);

	protected abstract void visitBrowser(Browser widget);

	protected abstract void visitCCombo(CCombo widget);

	protected abstract void visitCLabel(CLabel widget);

	protected abstract void visitCombo(Combo widget);

	protected abstract void visitCTabItem(CTabItem widget);

	protected abstract void visitDateTime(DateTime widget);

	protected abstract void visitExpandBar(ExpandBar widget);

	protected abstract void visitExpandItem(ExpandItem widget);

	protected abstract void visitLabel(Label widget);

	protected abstract void visitLink(Link widget);

	protected abstract void visitList(List widget);

	protected abstract void visitMenu(Menu widget);

	protected abstract void visitScale(Scale widget);

	protected abstract void visitShell(Shell widget);

	protected abstract void visitSlider(Slider widget);

	protected abstract void visitSpinner(Spinner widget);

	protected abstract void visitStyledText(StyledText widget);

	protected abstract void visitTabItem(TabItem widget);

	protected abstract void visitTable(Table widget);

	protected abstract void visitTableColumn(TableColumn widget);

	protected abstract void visitTableItem(TableItem widget);

	protected abstract void visitText(Text widget);

	protected abstract void visitToolBar(ToolBar widget);
	
	protected abstract void visitToolItem(ToolItem widget);

	protected abstract void visitTray(Tray widget);

	protected abstract void visitTree(Tree widget);

	protected abstract void visitTreeItem(TreeItem widget);
	
	protected abstract void visitHyperLink(Hyperlink widget);
	
	protected abstract void visitGroup(Group widget);

	protected abstract void visitUnkownItem(Widget widget);
}
