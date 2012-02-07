package org.jboss.tools.ui.bot.ext.logging;

import javax.swing.table.TableColumn;

import org.apache.log4j.Logger;
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
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTreeItem;
import org.eclipse.ui.forms.widgets.Hyperlink;

class LogWidgetsVisitor extends BasicWidgetsVisitor {

	private Logger log = Logger.getLogger(LogWidgetsVisitor.class);
	
	@Override
	protected void visitButton(Button widget) {
		log.info(widget);
	}

	@Override
	protected void visitBrowser(Browser widget) {
		log.info(widget);
	}

	@Override
	protected void visitCCombo(CCombo widget) {
		log.info(widget);
	}

	@Override
	protected void visitCLabel(CLabel widget) {
		log.info(widget);
	}

	@Override
	protected void visitCombo(Combo widget) {
		StringBuilder text = new StringBuilder();
		text.append("Combo {");
		text.append("selected = '");
		text.append(widget.getText());
		text.append("', ");
		text.append("items = {");
		for (String item : widget.getItems()){
			text.append(item + ", ");
		}
		text.append("}");
		text.append("}");
		log.info(text.toString());
	}

	@Override
	protected void visitCTabItem(CTabItem widget) {
		log.info(widget);
	}

	@Override
	protected void visitDateTime(DateTime widget) {
		log.info(widget);
	}

	@Override
	protected void visitExpandBar(ExpandBar widget) {
		log.info(widget);
	}

	@Override
	protected void visitExpandItem(ExpandItem widget) {
		log.info(widget);
	}

	@Override
	protected void visitLabel(Label widget) {
		log.info(widget);
	}

	@Override
	protected void visitLink(Link widget) {
		log.info(widget);
	}

	@Override
	protected void visitList(List widget) {
		log.info(widget);
	}

	@Override
	protected void visitMenu(Menu widget) {
		log.info(widget);
	}

	@Override
	protected void visitScale(Scale widget) {
		log.info(widget);
	}

	@Override
	protected void visitShell(Shell widget) {
		log.info(widget);
	}

	@Override
	protected void visitSlider(Slider widget) {
		log.info(widget);
	}

	@Override
	protected void visitSpinner(Spinner widget) {
		log.info(widget);
	}

	@Override
	protected void visitStyledText(StyledText widget) {
		log.info(widget);
	}

	@Override
	protected void visitTabItem(TabItem widget) {
		log.info(widget);
	}

	@Override
	protected void visitTable(Table widget) {
		log.info(widget);
	}

	@Override
	protected void visitTableColumn(TableColumn widget) {
		log.info(widget);
	}

	@Override
	protected void visitTableItem(TableItem widget) {
		log.info(widget);
	}

	@Override
	protected void visitText(Text widget) {
		StringBuilder text = new StringBuilder();
		text.append("Text {");
		text.append(widget.getText());
		text.append("}");
		log.info(text.toString());
	}

	@Override
	protected void visitToolBar(ToolBar widget) {
		StringBuilder text = new StringBuilder();
		text.append("Toolbar {");
		text.append(widget.getRowCount() + " row(s), ");
		text.append(widget.getItemCount() + " item(s)");
		text.append("}");
		log.info(text.toString());
	}

	@Override
	protected void visitToolItem(ToolItem widget) {
		StringBuilder text = new StringBuilder();
		text.append("Toolitem {");
		text.append("text = '" + widget.getText() + "', ");
		text.append("tooltip = '" + widget.getToolTipText() + "'");
		text.append("}");
		log.info(text.toString());
	}
	
	@Override
	protected void visitTray(Tray widget) {
		log.info(widget);
	}

	@Override
	protected void visitTree(Tree widget) {
		StringBuilder text = new StringBuilder();
		text.append("Tree {");
		text.append(widget.getItemCount() + " item(s), ");
		text.append(widget.getColumnCount() + " columns(s), ");
		text.append("items = {");
		for (SWTBotTreeItem item : new SWTBotTree(widget).getAllItems()){
			text.append(item.getText() + ", ");
		}
		text.append("}");
		text.append("}");
		log.info(text.toString());
	}

	@Override
	protected void visitTreeItem(TreeItem widget) {
		log.info(widget);
	}
	
	@Override
	protected void visitHyperLink(Hyperlink widget) {
		StringBuilder text = new StringBuilder();
		text.append("Hyperlink {");
		text.append("text = '" + widget.getText() + "'");
		text.append("}");
		log.info(text.toString());
	}
	
	@Override
	protected void visitGroup(Group widget) {
		log.info(widget);
	}

	@Override
	protected void visitUnkownItem(Widget widget) {
		log.info("Unknown: " + widget);
	}
}