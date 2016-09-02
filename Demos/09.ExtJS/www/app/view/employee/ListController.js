/**
 * This class is the controller for the main view for the application. It is specified as
 * the "controller" of the Main view class.
 *
 * TODO - Replace this content of this view to suite the needs of your application.
 */
Ext.define('SecondDemo.view.employee.ListController', {
    extend: 'Ext.app.ViewController',

    alias: 'controller.employee-list',

    init: function () {
        this.grid = this.getView();
        this.store = this.grid.getStore();
    },

    onItemSelected: function (sender, record) {
        Ext.Msg.confirm('Confirm', 'Are you sure?', 'onConfirm', this);
    },

    onSearchClick: function () {
        this.store.load();
    },

    onNewClick: function () {
        var record = this.store.insert(0, {});
        //this.grid.setSelection(record);
        //this.grid.getView().focusRow(this.store.count());
    },

    onDeleteClick: function () {
        var s = this.grid.getSelection();
        if (s.length > 0) {
            this.store.remove(s[0]);
        }
    },

    onSaveClick: function() {
        this.store.sync({
            failure: function (batch, options) {
                var response = batch.exceptions[0].getError().response,
                    jsonError = Ext.decode(response.responseText);
                Ext.Msg.alert('Server error', jsonError.message);
            }
        });
    },

    onCancelClick: function() {
        this.store.rejectChanges();
    },

    onConfirm: function (choice) {
        if (choice === 'yes') {
            //
        }
    },

    onPrintClick: function () {
        Ext.Msg.alert('Client error', 'Not yet implemented');
    }
    
});
